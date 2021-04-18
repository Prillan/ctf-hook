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
import           System.Random              (randomRIO)

import           CTF.Hook.Types             (SessionToken (..), User (..))
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

subdomainRequestKey :: ByteString -> ByteString
subdomainRequestKey subdomain = "ctf-hook/data/" <> subdomain

subdomainResponseKey :: ByteString -> ByteString
subdomainResponseKey subdomain = "ctf-hook/data/" <> subdomain <> "/responses"

touchKey :: (Applicative f, RedisCtx m f) => ByteString -> m (f ())
touchKey key = expire key dataTimeout *> emptyResponse

newSession :: User -> Redis SessionToken
newSession user = do
  token <- generateSessionToken
  fromCtx $ insertSessionToken token user
  pure token

generateSessionToken :: MonadIO m => m SessionToken
generateSessionToken = liftIO $
  SessionToken . fromString <$> replicateM 30 (randomRIO ('a', 'z'))

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

pushData :: ByteString -> ByteString -> Redis ()
pushData subdomain d = fromCtx $ rpush key [d] *> touchKey key
  where key = subdomainRequestKey subdomain

readData :: ByteString -> Redis (Maybe ByteString)
readData subdomain = fromCtx $ do
  let key = subdomainRequestKey subdomain
  d <- blpop [key] readBlockTimeout
  touchKey key
  pure $ fmap (fmap snd) d -- okay..

storeServedResponse :: ByteString
                    -> ByteString
                    -> ServableResponse
                    -> Redis ()
storeServedResponse subdomain path content = do
  let key = subdomainResponseKey subdomain
  fromCtx $ hset key path (encode content)
  fromCtx $ expire key dataTimeout
  fromCtx $ emptyResponse

fetchServedResponse :: ByteString
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
