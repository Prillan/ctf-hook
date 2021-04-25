module CTF.Hook.App where

import           Control.Monad                        (unless, when)
import           Control.Monad.Trans.Except           (runExceptT)
import           Control.Monad.Trans.Maybe            (MaybeT (MaybeT, runMaybeT))
import           Control.Monad.Trans.Random           (evalRandTIO)
import           Crypto.BCrypt                        (validatePassword)
import           Data.Aeson                           (Value (..), encode,
                                                       object, toJSON, (.=))
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy                 as LB
import           Data.Char                            (isDigit)
import           Data.String                          (fromString)
import qualified Data.Text
import           Database.Redis                       (Connection)
import           Network.HTTP.Types.Status            (status403, status500)
import           Network.Wai                          (Application, Request,
                                                       pathInfo, rawPathInfo,
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
                                                       Subdomain (..),
                                                       User (User))
import           CTF.Hook.Utils                       (magicContentType)
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
  S.get "/fetch/:subdomain" $ auth conn \user ->
    fetchView user conn

  {- Serves a file/files for :subdomain -}
  S.post "/serve/:subdomain" $ auth conn \user ->
    serveView user conn

  {- Route for the receiver, using /s/:subdomain/ instead -}
  S.matchAny subdomainInPathPattern $
    storeDataView conn


requestProperty :: (Request -> a) -> S.ActionM a
requestProperty prop = prop <$> S.request

redis :: Connection -> Redis a -> S.ActionM a
redis conn a = runExceptT (runRedis conn a)  >>= \case
  Right v -> pure v
  Left e -> handleError e

handleError :: Error -> S.ActionM a
handleError e =
  let (s, msg) = case e of
        RedisError _   -> (status500, "redis error")
        GeneralError m -> (status500, "unknown error: " <> m)
        Unauthorized m -> (status403, "unauthorized: " <> m)
  in S.status s
     *> S.json (object [ "errors" .= toJSON [msg] ])
     *> S.finish

storeDataView :: Connection -> S.ActionM ()
storeDataView r = do
  subdomain <- subdomainParam
  storableReq <- encode <$> parseRequest
  path <- requestProperty rawPathInfo
  response <- redis r do
    pushData subdomain (LB.toStrict storableReq)
    fetchServedResponse subdomain path
  case response of
    Just response' -> do
      S.setHeader "Content-Type" (convert $ srContentType response')
      S.raw (convert $ srContent response')
    Nothing -> do
      let sd = convert (unSubdomain subdomain)
      S.text $ fromString $ "data stored for subdomain " ++ sd ++ "!"

indexView :: S.ActionM ()
indexView = S.text "hello world!"

fetchView :: User -> Connection -> S.ActionM ()
fetchView user r = do
  subdomain <- subdomainParam
  response <- redis r $ readData user subdomain
  case response of
    Just d -> do
      S.setHeader "Content-Type" "application/json; charset=utf-8"
      S.raw (LB.fromStrict d)
    Nothing -> S.json Null

serveView :: User -> Connection -> S.ActionM ()
serveView user r = do
  subdomain <- subdomainParam
  path      <- appendSlash <$> S.param "path"
  (_, f):_  <- S.files
  let content = convert (fileContent f)
  contentType <- getFileContentType content
  redis r $ storeServedResponse user
                                subdomain
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
  user <- runMaybeT do
    token <- getRequestToken
    MaybeT . redis r $ sessionUser token
  maybe failure cont user

subdomainParam :: S.ActionM Subdomain
subdomainParam = Subdomain <$> S.param "subdomain"

loginView :: Foldable t => t ConfigUser -> Connection -> S.ActionM ()
loginView users r = do
  user <- S.param "user"
  pass <- S.param "password"
  requestedSubdomain <- (Just <$> subdomainParam) `S.rescue` \_ -> pure Nothing
  unless (verifyCredentials users user pass) do
    S.status status403
    S.json (str $ "Invalid username or password")
    S.finish

  (SessionToken token , Subdomain subdomain) <- login r (User (convert user)) requestedSubdomain
  S.json (object [ "token" .= str (convert token)
                 , "subdomain" .= str (convert subdomain) ])

login :: Connection
  -> User
  -> Maybe Subdomain
  -> S.ActionM (SessionToken, Subdomain)
login conn user subdomain =
  redis conn
  . evalRandTIO
  $ newSession user subdomain

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
  pure [("subdomain", fromString matched)]

subdomainInPathPattern :: S.RoutePattern
subdomainInPathPattern = S.function \req -> do
  ("s":subdomain:_) <- pure $ pathInfo req
  pure [("subdomain", convert subdomain)]

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
runApp config@Config{..} conn = S.scotty configPort do
  when configDebug (S.middleware logStdoutDev)
  app' config conn
