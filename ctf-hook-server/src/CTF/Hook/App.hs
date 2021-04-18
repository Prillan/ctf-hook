{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module CTF.Hook.App where -- (runApp, app) where

import           Control.Monad                        (join)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Except           (runExceptT)
import           Control.Monad.Trans.Maybe            (MaybeT (MaybeT, runMaybeT))
import           Crypto.BCrypt                        (validatePassword)
import           Data.Aeson                           (Value (..), encode)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy                 as LB
import           Data.Char                            (isDigit)
import           Data.String                          (fromString)
import qualified Data.Text
import qualified Data.Text.Encoding                   as E
import           Database.Redis                       (Connection,
                                                       Reply (Error))
import           Network.HTTP.Types.Status            (status403, status500)
import           Network.Wai                          (Application, Request,
                                                       rawPathInfo,
                                                       requestHeaderHost)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Parse                    (FileInfo (..))
import qualified Web.Scotty                           as S

import           CTF.Hook.Config                      (Config (..),
                                                       ConfigUser (..))
import           CTF.Hook.Convert                     (convert, toString)
import           CTF.Hook.DB
import           CTF.Hook.Request                     (parseRequest)
import           CTF.Hook.Types                       (SessionToken (..),
                                                       User (User))
import           CTF.Hook.Utils                       (eitherToMaybe,
                                                       magicContentType)
{-|
Two parts:

Receiver:
  The receiver should receive data and store it for read later. Should probably
  store it in redis with TTL.

Fetcher:
  Fetches all data from a subdomain receiver.

Future things:
  - Send response (2-way communication) or set pre-defined response (html/json)
  - Pull all responses immediately
|-}

str :: Data.Text.Text -> Value
str = Data.Aeson.String

app' :: Config -> Connection -> S.ScottyM ()
app' Config{..} conn = do
  {- Route for the receiver, any data posted is stored here -}
  S.matchAny (subdomainRoutePattern configDomainPattern) $
    storeDataView conn

  {- Hello world -}
  S.get "/" $
    indexView

  {- Logs in the user to get a new session token -}
  S.post "/login" $
    loginView configUsers conn

  {- Fetches data for :subdomain -}
  S.get "/fetch/:subdomain" $ auth conn $ \_ ->
    fetchView conn

  {- Serves a file/files for :subdomain -}
  S.post "/serve/:subdomain" $ auth conn $ \_ ->
    serveView conn

requestProperty :: (Request -> a) -> S.ActionM a
requestProperty prop = prop <$> S.request

redis :: MonadIO m => Connection -> Redis a -> m (Either Reply a)
redis conn = runExceptT . runRedis conn

storeDataView :: Connection -> S.ActionM ()
storeDataView r = do
  subdomain <- S.param "subdomain"
  storableReq <- encode <$> parseRequest
  path <- requestProperty rawPathInfo
  response <- redis r $ do
    pushData (fromString subdomain) (LB.toStrict storableReq)
    fetchServedResponse (fromString subdomain) path

  liftIO $ print $ response
  case response of
    Right (Just response') -> do
      S.setHeader "Content-Type" (convert $ srContentType response')
      S.raw (convert $ srContent response')
    Right Nothing -> do
      S.text $ fromString $ "data stored for subdomain " ++ subdomain ++ "!"
    Left _ -> do
      S.status status500
      S.json (str $ "other redis error")

indexView :: S.ActionM ()
indexView = S.text "hello world!"

fetchView :: Connection -> S.ActionM ()
fetchView r = do
  subdomain <- S.param "subdomain"
  response <- redis r $ readData subdomain
  case response of
    Right (Just d) -> do
      S.setHeader "Content-Type" "application/json; charset=utf-8"
      S.raw (LB.fromStrict d)
    Right Nothing  -> S.json Null
    Left (Error e) -> do
      S.status status500
      S.json (str $ "redis error: " <> E.decodeUtf8 e)
    Left _ -> do
      S.status status500
      S.json (str $ "other redis error")

serveView :: Connection -> S.ActionM ()
serveView r = do
    subdomain <- S.param "subdomain"
    path      <- appendSlash <$> S.param "path"
    (_, f):_ <- S.files
    let content = convert (fileContent f)
    contentType <- getFileContentType content
    redis r $ storeServedResponse subdomain
                                  path
                                  ServableResponse { srContentType = contentType
                                                   , srContent = content }
    S.json (str $ "Files stored")

appendSlash :: B.ByteString -> B.ByteString
appendSlash x =
  case B.take 1 x of
    "/" -> x
    "*" -> x
    _   -> "/" <> x

getFileContentType :: B.ByteString -> S.ActionM String
getFileContentType content =
  S.param "content-type" `S.rescue` \_ ->
    magicContentType content

auth :: Connection -> (User -> S.ActionM ()) -> S.ActionM ()
auth r cont = do
  let failure = S.status status403 *> S.json (str "Failed to validate session token")
  user <- runMaybeT $ do
    token <- getRequestToken
    MaybeT
      . fmap (join . eitherToMaybe)
      . redis r
      $ sessionUser token
  maybe failure cont user

loginView :: Foldable t => t ConfigUser -> Connection -> S.ActionM ()
loginView users r = do
  user <- S.param "user"
  pass <- S.param "password"
  if verifyCredentials users user pass
    then do
      Right (SessionToken token) <- redis r $ newSession $ User (convert user)
      S.json (str $ convert token)
    else do
      S.status status403
      S.json (str $ "Invalid username or password")

getRequestToken :: MaybeT S.ActionM SessionToken
getRequestToken = do
  ["Bearer", token] <- words . convert <$> MaybeT (S.header "Authorization")
  pure (fromString token)

subdomainRoutePattern :: String -> S.RoutePattern
subdomainRoutePattern domainPattern =
  S.function $ subdomainRoutePattern' domainPattern

subdomainRoutePattern' :: String -> Request -> Maybe [S.Param]
subdomainRoutePattern' domainPattern req = do
  hostHeader <- requestHeaderHost req
  let domain = domainFromHost (toString hostHeader)
  matched <- subdomainMatch domainPattern domain
  pure $ [("subdomain", fromString matched)]

subdomainMatch :: String -> String -> Maybe String
subdomainMatch domainPattern host =
  let p = drop 1 domainPattern
      h = dropWhile ('.' /=) host
  in
    if h == p
       then Just $ takeWhile ('.' /=) host
       else Nothing

domainFromHost :: String -> String
domainFromHost = stripPort

stripPort :: String -> String
stripPort host =
  case dropWhile isDigit (reverse host) of
    ':':rest -> reverse rest
    _        -> host

verifyCredentials :: Foldable t
                  => t ConfigUser
                  -> String
                  -> String
                  -> Bool
verifyCredentials users u p = any check users
  where check (ConfigUser u' h) =
          u == u' && validatePassword (fromString h) (fromString p)

app :: Config -> Connection -> IO Application
app config conn = S.scottyApp $ app' config conn

runApp :: Config -> Connection -> IO ()
runApp config@Config{..} conn = S.scotty configPort $ do
  if configDebug
    then S.middleware $ logStdoutDev
    else pure ()
  app' config conn
