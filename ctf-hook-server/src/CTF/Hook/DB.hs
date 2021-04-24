{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module CTF.Hook.DB ( Redis
                   , ServableResponse(..)
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
import           Control.Monad.Random       (evalRandIO, uniform)

import           CTF.Hook.Types             ( SessionToken (..)
                                            , Subdomain (..)
                                            , User (..))
import           CTF.Hook.Utils             (eitherToMaybe)

-- This is the type exposed to the rest of the library/program,
-- internally in this module we might use (RedisCtx m f) => m (f a)
-- sometimes, but don't expose it.
newtype Redis a = Redis (ExceptT Reply R.Redis a)
  deriving (Functor, Applicative, Monad, MonadIO)

runRedis :: MonadIO m => Connection -> Redis a -> ExceptT Reply m a
runRedis c (Redis x) = mapExceptT (liftIO . R.runRedis c) x

-- Forces
--     (m ~ R.Redis, f ~ Either Reply)
-- in
--     RedisCtx m f => m (f a)
-- So that we can do things like
--     fromCtx (get "a") :: Redis (Maybe ByteString)
-- instead of
--     get "a" :: RedisCtx m f => m (f a)
fromCtx :: R.Redis (Either Reply a) -> Redis a
fromCtx = Redis . ExceptT

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

newSession :: User
  -> Maybe Subdomain
  -> Redis (Maybe (SessionToken, Subdomain))
newSession user subdomain = do
  token <- generateSessionToken
  fromCtx $ insertSessionToken token user
  case subdomain of
    Just subdomain' -> subdomainOwner subdomain' >>= \case
      Just owner | owner == user -> pure (Just (token, subdomain'))
      -- If the owner is a different user, fail.
      --
      -- If the subdomain isn't registered, fail (we don't
      -- want users to generate their own names atm).
      _ -> pure Nothing
    Nothing -> do
      subdomain' <- generateSubdomain
      fromCtx $ registerSubdomain user subdomain'
      pure (Just (token, subdomain'))

generateSubdomain :: MonadIO m => m Subdomain
generateSubdomain = liftIO . evalRandIO $
  fromString <$> replicateM 10 (uniform $ ['a'..'z'] ++ ['0'..'9'])

generateSessionToken :: MonadIO m => m SessionToken
generateSessionToken = liftIO . evalRandIO $
  fromString <$> replicateM 30 (uniform ['a'..'z'])

registerSubdomain :: (Applicative f, RedisCtx m f)
                  => User
                  -> Subdomain
                  -> m (f ())
registerSubdomain (User user) subdomain = do
  let key = ownerKey subdomain
  set key user
  expire key ownerTimeout
  pure (pure ())

subdomainOwner :: Subdomain
               -> Redis (Maybe User)
subdomainOwner subdomain = do
  let key = ownerKey subdomain
  fmap User <$> fromCtx (get key <* expire key ownerTimeout)

insertSessionToken :: (Applicative f, RedisCtx m f)
                   => SessionToken
                   -> User
                   -> m (f ())
insertSessionToken token (User user) = do
  let key = sessionKey token
  set key user
  expire key sessionTimeout
  emptyResponse

sessionUser :: SessionToken -> Redis (Maybe User)
sessionUser token =
  let key = sessionKey token
  in fromCtx (get key) >>= \case
    Just v -> fromCtx (expire key sessionTimeout) >> pure (Just (User v))
    _ -> pure Nothing

pushData :: Subdomain -> ByteString -> Redis ()
pushData subdomain d = fromCtx $ rpush key [d] *> touchKey key
  where key = subdomainRequestKey subdomain

readData :: User -> Subdomain -> Redis (Maybe ByteString)
readData user subdomain =
  let key = subdomainRequestKey subdomain
  in
    subdomainOwner subdomain >>= \case
      Just owner | owner == user -> do
        d <- fromCtx $ blpop [key] readBlockTimeout
        fromCtx $ touchKey key
        pure $ snd <$> d
      _ -> pure Nothing -- TODO: Provide error message

storeServedResponse :: User
                    -> Subdomain
                    -> ByteString
                    -> ServableResponse
                    -> Redis (Maybe ())
storeServedResponse user subdomain path content =
  subdomainOwner subdomain >>= \case
    Just owner | owner == user -> do
        let key = subdomainResponseKey subdomain
        fromCtx $ hset key path (encode content)
        fromCtx $ expire key dataTimeout
        pure (Just ())
    _ -> pure Nothing -- TODO: Provide error message


fetchServedResponse :: Subdomain
                    -> ByteString
                    -> Redis (Maybe ServableResponse)
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
