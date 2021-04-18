{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
module Main (main) where

import           Control.Monad             (forM_)
import           Data.Aeson                (Value (..), decode, object, (.=))
import qualified Data.ByteString.Lazy      as B
import           Data.Either               (isRight)
import           Network.HTTP.Types.Header
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           CTF.Hook.App            (stripPort, subdomainMatch)
import           CTF.Hook.Config         (Config (..), ConfigUser (..),
                                          decodeConfig)
import           CTF.Hook.Utils          (magicContentType)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "magicContentType" $ do
    let cases = [ ("this is some text", "text/plain")
                , ("BZh91AY..........", "application/x-bzip2")
                , ("[1,2,3,4,5,6,7,8]", "application/json")]
    forM_ cases $ \(input, expected) ->
      it ("should identify an example " ++ show expected ++ " content type") $ do
        magicContentType input `shouldReturn` expected

  describe "stripPort" $ do
    let cases = [ ("localhost", "localhost")
                , ("localhost:1234", "localhost")
                , ("7p4zsn9hdn.127.0.0.1.xip.io:8080", "7p4zsn9hdn.127.0.0.1.xip.io") ]
    forM_ cases $ \(input, expected) ->
      it ("should work correctly for " ++ input) $ do
        stripPort input `shouldBe` expected

  describe "subdomainMatch" $ do
    it "matches the example domain pattern" $ do
      subdomainMatch "*.127.0.0.1.xip.io" "9d6p72o2z4.127.0.0.1.xip.io" `shouldBe` Just "9d6p72o2z4"
    it "matches an example subdomain" $ do
      subdomainMatch "*.test" "asdf.test" `shouldBe` (Just "asdf")
    it "doesn't match a subdomain" $ do
      subdomainMatch "*.asdf" "xxxxxx" `shouldBe` Nothing

  describe "Config json parser" $ do
    let user = ConfigUser "testname" "testhash"
        config = Config { configUsers = [user]
                        , configDomainPattern = "pattern"
                        , configPort = 1234
                        , configDebug = False }
    it "should parse a valid user" $ do
      let j = "{\"name\": \"testname\", \"hash\": \"testhash\"}"
      decode j `shouldBe` Just user
    it "should parse a valid config" $ do
      let j = "{ \
              \  \"users\": [{\"name\": \"testname\", \"hash\": \"testhash\"}], \
              \  \"domain_pattern\": \"pattern\",  \
              \  \"port\": 1234,  \
              \  \"debug\": false  \
              \}"
      decode j `shouldBe` Just config

  describe "The example config" $ do
    it "should parse" $ do
      content <- B.readFile "example.config.json"
      decodeConfig content `shouldSatisfy` isRight

  -- TODO: Add tests for the app
  -- TODO: Requires refactoring of the redis stuff, again.
  -- with app $ do
  -- describe "GET /" $ do
  --   it "responds with 200" $ do
  --     get "/" `shouldRespondWith` 200

--     it "responds with 'hello'" $ do
--       get "/" `shouldRespondWith` "hello"

--     it "responds with 200 / 'hello'" $ do
--       get "/" `shouldRespondWith` "hello" {matchStatus = 200}

--     it "has 'Content-Type: text/plain; charset=utf-8'" $ do
--       get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

--   describe "GET /some-json" $ do
--     it "responds with some JSON" $ do
--       get "/some-json" `shouldRespondWith` expectedJsonResponse

-- expectedJsonResponse =
--   let ResponseMatcher status headers body = [json|{foo: 23, bar: 42}|]
--   in ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body
