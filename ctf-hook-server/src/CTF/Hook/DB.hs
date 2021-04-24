{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
module CTF.Hook.DB ( Redis
                   , ServableResponse(..)
                   , Error(..)
                   , sessionUser
                   , fetchServedResponse
                   , newSession
                   , pushData
                   , readData
                   , storeServedResponse
                   , runRedis ) where

import           Control.Applicative        (Alternative (empty, (<|>)))
import           Control.Monad              (join, replicateM)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Random       (MonadRandom, uniform)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Except (ExceptT (..), mapExceptT)
import           Data.ByteString            (ByteString)
import           Data.Data                  (Data, Typeable)
import           Data.Serialize             (Serialize, decode, encode)
import           Data.String                (fromString)
import           Database.Redis             (Connection, RedisCtx, Reply, blpop,
                                             expire, get, hget, hset, rpush,
                                             set)
import qualified Database.Redis             as R
import           GHC.Generics               (Generic)

import           CTF.Hook.Convert           (convert)
import           CTF.Hook.Types             (SessionToken (..), Subdomain (..),
                                             User (..))
import           CTF.Hook.Utils             (eitherToMaybe)


data Error = RedisError ByteString
           | GeneralError String
  deriving Eq

instance Show Error where
  show = \case
    RedisError e -> "redis error: " <> convert e
    GeneralError e -> e

-- This is the type exposed to the rest of the library/program,
-- internally in this module we might use (RedisCtx m f) => m (f a)
-- sometimes, but don't expose it.
newtype Redis a = Redis (ExceptT [Error] R.Redis a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadFail Redis where
  fail = Redis . ExceptT . pure . Left . pure . GeneralError

class Monad m => MonadRedis m where
  fromCtx :: R.Redis (Either Reply a) -> m a

instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadRedis m
  ) => MonadRedis (t m) where
  fromCtx = lift . fromCtx

runRedis :: MonadIO m => Connection -> Redis a -> ExceptT [Error] m a
runRedis c (Redis x) = mapExceptT (liftIO . R.runRedis c) x

-- Forces
--     (m ~ R.Redis, f ~ Either Reply)
-- in
--     RedisCtx m f => m (f a)
-- So that we can do things like
--     fromCtx (get "a") :: Redis (Maybe ByteString)
-- instead of
--     get "a" :: RedisCtx m f => m (f a)
instance MonadRedis Redis where
  fromCtx m = Redis . ExceptT $ m >>= \case
    Right v          -> pure (Right v)
    Left (R.Error e) -> pure (Left [RedisError e])
    Left _           -> pure (Left [GeneralError "invalid redis error"])

-- TODO: Make the timeouts configurable
sessionTimeout :: Integer
sessionTimeout = 30 * 60

dataTimeout :: Integer
dataTimeout = 60 * 60

ownerTimeout :: Integer
ownerTimeout = dataTimeout * 10

readBlockTimeout :: Integer
readBlockTimeout = 10

data ServableResponse = ServableResponse { srContentType :: String
                                         , srContent     :: ByteString }
  deriving (Show, Eq, Data, Typeable, Generic)

instance Serialize ServableResponse

emptyResponse :: (Applicative f, RedisCtx m f) => m (f ())
emptyResponse = pure (pure ())

sessionKey :: SessionToken -> ByteString
sessionKey (SessionToken token) = "ctf-hook/session/" <> token

subdomainRequestKey :: Subdomain -> ByteString
subdomainRequestKey (Subdomain subdomain) =
  "ctf-hook/data/" <> subdomain

subdomainResponseKey :: Subdomain -> ByteString
subdomainResponseKey (Subdomain subdomain) =
  "ctf-hook/data/" <> subdomain <> "/responses"

ownerKey :: Subdomain -> ByteString
ownerKey (Subdomain subdomain) = "ctf-hook/owner/" <> subdomain

touchKey :: (Applicative f, RedisCtx m f) => ByteString -> m (f ())
touchKey key = expire key dataTimeout *> emptyResponse

newSession :: (MonadFail m, MonadRedis m, MonadRandom m)
  => User
  -> Maybe Subdomain
  -> m (SessionToken, Subdomain)
newSession user subdomain = do
  token <- generateSessionToken
  fromCtx $ insertSessionToken token user
  case subdomain of
    Just subdomain' ->
      validateOwner user subdomain' *> pure (token, subdomain')
    Nothing -> do
      subdomain' <- generateSubdomain
      fromCtx $ registerSubdomain user subdomain'
      pure (token, subdomain')

generateSubdomain :: (MonadRedis m, MonadRandom m) => m Subdomain
generateSubdomain = do
  candidate <- fromString <$> replicateM 10 (uniform $ ['a'..'z'] ++ ['0'..'9'])
  let key = ownerKey candidate
  fromCtx (get key) >>= \case
    Just _  -> generateSubdomain
    Nothing -> pure candidate

generateSessionToken :: MonadRandom m => m SessionToken
generateSessionToken =
  fromString <$> replicateM 30 (uniform ['a'..'z'])

registerSubdomain :: (Applicative f, RedisCtx m f)
                  => User
                  -> Subdomain
                  -> m (f ())
registerSubdomain (User user) subdomain =
  set key user *> expire key ownerTimeout *> emptyResponse
  where key = ownerKey subdomain

subdomainOwner :: MonadRedis m
               => Subdomain
               -> m (Maybe User)
subdomainOwner subdomain = do
  let key = ownerKey subdomain
  fmap User <$> fromCtx (get key <* expire key ownerTimeout)

insertSessionToken :: (Applicative f, RedisCtx m f)
                   => SessionToken
                   -> User
                   -> m (f ())
insertSessionToken token (User user) =
  set key user *> expire key sessionTimeout *> emptyResponse
  where key = sessionKey token

sessionUser :: SessionToken -> Redis (Maybe User)
sessionUser token =
  let key = sessionKey token
  in fromCtx (get key) >>= \case
    Just v -> fromCtx (expire key sessionTimeout) >> pure (Just (User v))
    _ -> pure Nothing

pushData :: Subdomain -> ByteString -> Redis ()
pushData subdomain d = fromCtx $ rpush key [d] *> touchKey key
  where key = subdomainRequestKey subdomain

readData :: (MonadFail m, MonadRedis m)
  => User
  -> Subdomain
  -> m (Maybe ByteString)
readData user subdomain = do
  validateOwner user subdomain
  let key = subdomainRequestKey subdomain
  d <- fromCtx $ blpop [key] readBlockTimeout
  fromCtx $ touchKey key
  pure $ snd <$> d

storeServedResponse :: (MonadFail m, MonadRedis m)
                    => User
                    -> Subdomain
                    -> ByteString
                    -> ServableResponse
                    -> m (Maybe ())
storeServedResponse user subdomain path content = do
  validateOwner user subdomain
  let key = subdomainResponseKey subdomain
  fromCtx $ hset key path (encode content)
  fromCtx $ expire key dataTimeout
  pure (Just ())

validateOwner :: (MonadFail m, MonadRedis m)
  => User
  -> Subdomain
  -> m ()
validateOwner user subdomain =
  subdomainOwner subdomain >>= \case
    Just owner | owner == user -> pure ()
    _ -> fail "subdomain doesn't exist, or not the owner"

fetchServedResponse :: MonadRedis m
                    => Subdomain
                    -> ByteString
                    -> m (Maybe ServableResponse)
fetchServedResponse subdomain path = do
  let key = subdomainResponseKey subdomain
      x = fromCtx $ do
        responses <- traverse (tryFetchResponse key) [path, "*"]
        pure . pure . tryAll . map (join . eitherToMaybe) $ responses
  fromCtx $ expire key dataTimeout >> emptyResponse
  fmap (>>= decodeMaybe) x

decodeMaybe :: Serialize a => ByteString -> Maybe a
decodeMaybe d =
  case decode d of
    Right x -> Just x
    Left _  -> Nothing

tryAll :: Alternative f => [f a] -> f a
tryAll []     = empty
tryAll (x:xs) = x <|> tryAll xs

tryFetchResponse :: RedisCtx m f => ByteString -> ByteString -> m (f (Maybe ByteString))
tryFetchResponse key field = hget key field
