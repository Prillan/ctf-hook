module CTF.Hook.Request ( StorableRequest(..)
                        , parseRequest ) where

import           Data.Aeson                  (FromJSON, ToJSON, Value)
import           Data.Bifunctor              (bimap)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Base64.Lazy as B64
import           Data.Data                   (Data, Typeable)
import           Data.Foldable               (fold)
import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8')
import           GHC.Generics                (Generic)
import           Network.Wai                 (pathInfo, rawPathInfo,
                                              rawQueryString, remoteHost,
                                              requestMethod)
import           Web.Scotty                  (ActionM, Param, rescue)
import qualified Web.Scotty                  as Scotty
import           Witch                       (into)

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
    <$> (pure . fold . decodeUtf8' . requestMethod $ req)
    <*> (pure . pathInfo $ req)
    <*> (drop 1 <$> Scotty.params) -- we drop the "subdomain" one
    <*> (pure . into @Text . show . remoteHost $ req)
    <*> Scotty.jsonData `rescue` (const (pure Nothing))
    <*> (map (bimap (into @Text) (into @Text)) <$> Scotty.headers)
    <*> (pure . fold . decodeUtf8' . rawPathInfo $ req)
    <*> (pure . fold . decodeUtf8' . rawQueryString $ req)
    <*> (fold . decodeUtf8' . into @ByteString . B64.encode <$> Scotty.body)
