{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CTF.Hook.Types ( User(..)
                      , SessionToken(..) ) where

import           Data.ByteString (ByteString)
import           Data.String     (IsString)

newtype User = User { unUser :: ByteString  }
  deriving (Show, Eq)

newtype SessionToken = SessionToken { unToken :: ByteString  }
  deriving (Show, Eq, IsString)
