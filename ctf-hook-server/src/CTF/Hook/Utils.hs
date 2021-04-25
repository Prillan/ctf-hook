module CTF.Hook.Utils ( eitherToMaybe
                      , magicContentType ) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString        as B
import           Magic.Data             (MagicFlag (MagicMimeType))
import           Magic.Init             (magicLoadDefault, magicOpen)
import           Magic.Operations       (magicCString)


eitherToMaybe :: Either l r -> Maybe r
eitherToMaybe (Right x) = Just x
eitherToMaybe _         = Nothing

magicContentType :: MonadIO m => B.ByteString -> m String
magicContentType content = liftIO do
  magic <- magicOpen [MagicMimeType]
  magicLoadDefault magic
  B.useAsCStringLen content (magicCString magic)
