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

import           Control.Monad              (replicateM)
import           Control.Monad.Except       (MonadError (throwError))
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Random       (MonadRandom, uniform)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Except (ExceptT (..), mapExceptT)
import           Data.ByteString            (ByteString)
import           Data.Data                  (Data, Typeable)
import           Data.Foldable              (asum, fold)
import           Data.Serialize             (Serialize, decode, encode)
import           Data.String                (fromString)
import           Data.Text.Encoding         (decodeUtf8')
import           Database.Redis             (Connection, RedisCtx, Reply, blpop,
                                             expire, get, hget, hset, rpush,
                                             set)
import qualified Database.Redis             as R
import           GHC.Generics               (Generic)
import           Witch                      (into)

import           CTF.Hook.Types             (SessionToken (..), Subdomain (..),
                                             User (..))


data Error = RedisError ByteString
           | Unauthorized String
           | GeneralError String
  deriving Eq

instance Show Error where
  show = \case
    RedisError e -> "redis error: " <> (into @String . fold . decodeUtf8' $ e)
    Unauthorized e -> "unauthorized: " <> e
    GeneralError e -> e

-- This is the type exposed to the rest of the library/program,
-- internally in this module we might use (RedisCtx m f) => m (f a)
-- sometimes, but don't expose it.
newtype Redis a = Redis (ExceptT Error R.Redis a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Error)

instance MonadFail Redis where
  fail = Redis . ExceptT . pure . Left . GeneralError

class Monad m => MonadRedis m where
  liftRedis :: R.Redis (Either Reply a) -> m a

instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadRedis m
  ) => MonadRedis (t m) where
  liftRedis = lift . liftRedis

runRedis :: MonadIO m => Connection -> Redis a -> ExceptT Error m a
runRedis c (Redis x) = mapExceptT (liftIO . R.runRedis c) x

-- Forces
--     (m ~ R.Redis, f ~ Either Reply)
-- in
--     RedisCtx m f => m (f a)
-- So that we can do things like
--     liftRedis (get "a") :: Redis (Maybe ByteString)
-- instead of
--     get "a" :: RedisCtx m f => m (f a)
instance MonadRedis Redis where
  liftRedis m = Redis . ExceptT $ m >>= \case
    Right v          -> pure (Right v)
    Left (R.Error e) -> pure (Left (RedisError e))
    Left _           -> pure (Left (GeneralError "invalid redis error"))

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

newSession :: (MonadError Error m, MonadRedis m, MonadRandom m)
  => User
  -> Maybe Subdomain
  -> m (SessionToken, Subdomain)
newSession user subdomain = do
  token <- generateSessionToken
  liftRedis $ insertSessionToken token user
  case subdomain of
    Just subdomain' ->
      validateOwner user subdomain' *> pure (token, subdomain')
    Nothing -> do
      subdomain' <- generateSubdomain
      liftRedis $ registerSubdomain user subdomain'
      pure (token, subdomain')

generateSubdomain :: (MonadRedis m, MonadRandom m) => m Subdomain
generateSubdomain = do
  candidate <- fromString <$> replicateM 10 (uniform $ ['a'..'z'] ++ ['0'..'9'])
  let key = ownerKey candidate
  liftRedis (get key) >>= \case
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
  fmap User <$> liftRedis (get key <* expire key ownerTimeout)

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
  in liftRedis (get key) >>= \case
    Just v -> liftRedis (expire key sessionTimeout) >> pure (Just (User v))
    _      -> pure Nothing

pushData :: Subdomain -> ByteString -> Redis ()
pushData subdomain d = liftRedis $ rpush key [d] *> touchKey key
  where key = subdomainRequestKey subdomain

readData :: (MonadError Error m, MonadRedis m)
  => User
  -> Subdomain
  -> m (Maybe ByteString)
readData user subdomain = do
  validateOwner user subdomain
  let key = subdomainRequestKey subdomain
  d <- liftRedis $ blpop [key] readBlockTimeout
  liftRedis $ touchKey key
  pure $ snd <$> d

storeServedResponse :: (MonadError Error m, MonadRedis m)
                    => User
                    -> Subdomain
                    -> ByteString
                    -> ServableResponse
                    -> m (Maybe ())
storeServedResponse user subdomain path content = do
  validateOwner user subdomain
  let key = subdomainResponseKey subdomain
  liftRedis $ hset key path (encode content)
  liftRedis $ expire key dataTimeout
  pure (Just ())

validateOwner :: (MonadError Error m, MonadRedis m)
  => User
  -> Subdomain
  -> m ()
validateOwner user subdomain =
  subdomainOwner subdomain >>= \case
    Just owner | owner == user -> pure ()
    _ -> throwError (Unauthorized "subdomain doesn't exist, or not the owner")

fetchServedResponse :: MonadRedis m
                    => Subdomain
                    -> ByteString
                    -> m (Maybe ServableResponse)
fetchServedResponse subdomain path = do
  let key = subdomainResponseKey subdomain
  responses <- traverse (liftRedis . tryFetchResponse key) [path, "*"]
  liftRedis $ expire key dataTimeout
  pure $ asum . map (>>= decodeMaybe) $ responses

decodeMaybe :: Serialize a => ByteString -> Maybe a
decodeMaybe d =
  case decode d of
    Right x -> Just x
    Left _  -> Nothing

tryFetchResponse :: RedisCtx m f => ByteString -> ByteString -> m (f (Maybe ByteString))
tryFetchResponse key field = hget key field
