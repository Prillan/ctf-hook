{-# LANGUAGE DeriveGeneric #-}
module CTF.Hook.Config ( Config(..)
                       , ConfigUser(..)
                       , decodeConfig ) where

import           Data.Aeson           (FromJSON (parseJSON), Options, camelTo2,
                                       defaultOptions, eitherDecode,
                                       fieldLabelModifier, genericParseJSON)
import           Data.ByteString.Lazy (ByteString)
import           GHC.Generics         (Generic)

data ConfigUser = ConfigUser { userName :: String
                             , userHash :: String }
  deriving (Show, Read, Eq, Generic)

data Config = Config { configUsers         :: [ConfigUser]
                     , configDomainPattern :: String
                     , configPort          :: Int
                     , configDebug         :: Bool }
  deriving (Show, Read, Eq, Generic)

optionsFromPrefix :: String -> Options
optionsFromPrefix prefix = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop (length prefix) }

instance FromJSON ConfigUser where
  parseJSON = genericParseJSON (optionsFromPrefix "user")

instance FromJSON Config where
  parseJSON = genericParseJSON (optionsFromPrefix "config")


decodeConfig :: ByteString -> Either String Config
decodeConfig = eitherDecode
