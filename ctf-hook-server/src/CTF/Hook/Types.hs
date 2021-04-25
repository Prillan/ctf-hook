module CTF.Hook.Types ( User(..)
                      , SessionToken(..)
                      , Subdomain(..) ) where

import           Data.ByteString (ByteString)
import           Data.String     (IsString)

newtype User = User { unUser :: ByteString  }
  deriving (Show, Eq)

newtype SessionToken = SessionToken { unToken :: ByteString  }
  deriving (Show, Eq, IsString)

newtype Subdomain = Subdomain { unSubdomain :: ByteString }
  deriving (Show, Eq, IsString)
