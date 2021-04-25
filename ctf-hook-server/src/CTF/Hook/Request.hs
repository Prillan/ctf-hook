module CTF.Hook.Request ( StorableRequest(..)
                        , parseRequest ) where

import           CTF.Hook.Convert            (convert)
import           Data.Aeson                  (FromJSON, ToJSON, Value)
import qualified Data.ByteString.Base64.Lazy as B64
import           Data.Data                   (Data, Typeable)
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           Network.Wai                 (pathInfo, rawPathInfo,
                                              rawQueryString, remoteHost,
                                              requestMethod)
import           Web.Scotty                  (ActionM, Param, rescue)
import qualified Web.Scotty                  as Scotty

data StorableRequest =
  StorableRequest { method           :: Text
                  , path             :: [Text]
                  , params           :: [Param]
                  , remote_host      :: Text
                  , json_body        :: Maybe Value
                  , headers          :: [(Text, Text)]
--                  , files :: [Text]
                  , raw_path         :: Text
                  , raw_query_string :: Text
                  , raw_body         :: Text }
  deriving (Eq, Show, Typeable, Data, Generic)

instance ToJSON StorableRequest
instance FromJSON StorableRequest

parseRequest :: ActionM StorableRequest
parseRequest = do
  req <- Scotty.request
  StorableRequest
    <$> pure (convert (requestMethod req))
    <*> pure (pathInfo req)
    <*> (drop 1 <$> Scotty.params) -- we drop the "subdomain" one
    <*> pure (convert (show (remoteHost req)))
    <*> Scotty.jsonData `rescue` (const (pure Nothing))
    <*> (map (\(k, v) -> (convert k, convert v)) <$> Scotty.headers)
    <*> pure (convert (rawPathInfo req))
    <*> pure (convert (rawQueryString req))
    <*> (convert . B64.encode <$> Scotty.body)
