{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           Control.Exception          (SomeException, try)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), except,
                                             runExceptT, withExceptT)
import qualified Data.ByteString.Lazy       as LB (readFile)
import           Database.Redis             (checkedConnect, defaultConnectInfo,
                                             disconnect)
import           System.Environment         (getEnv)

import           CTF.Hook.App               (runApp)
import           CTF.Hook.Config            (Config, decodeConfig)

readConfig :: ExceptT String IO Config
readConfig = do
  cf <- withExceptT (show @SomeException)
        . ExceptT
        . try
        $ getEnv "CTF_HOOK_CONFIG"
  contents <- lift $ LB.readFile cf
  except $ decodeConfig contents

main :: IO ()
main = do
  conf <- runExceptT readConfig
  case conf of
    Right conf' -> do
      conn <- checkedConnect defaultConnectInfo
      runApp conf' conn
      disconnect conn
    Left e -> putStrLn $ "Error: " ++ e
