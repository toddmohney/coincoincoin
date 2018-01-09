{-# LANGUAGE TemplateHaskell #-}

module Truffle.TypesSpec (main, spec) where

import           Control.Exception (throwIO)
import qualified Data.Aeson as AE
import           Data.ByteString.Char8 (ByteString)
import qualified Data.HashMap.Strict as HM
import           Data.FileEmbed (embedFile)
import qualified Data.Text as T
import           Test.Hspec

import CoinCoinCoin.Errors (ParseError(..))
import Truffle.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "BuildArtifact's FromJSON instance" $
        it "parses the build artifact json" $
            case (AE.eitherDecodeStrict contractArtifactPayload :: Either String BuildArtifact) of
                Left err -> throwIO (JSONParseError $ T.pack err)
                Right artifact -> do
                    contractName artifact `shouldBe` "Hellos"
                    HM.size (networks artifact) `shouldBe` 11
                    HM.lookup "15" (networks artifact) `shouldBe` Just (Network "0x0ed41af5ecc84a9a4a46127e255c56c6f27f89fd")
                    HM.lookup "1978" (networks artifact) `shouldBe` Just (Network "0x9b24481161d616539e6f80827ea4315801027bb2")

contractArtifactPayload :: ByteString
contractArtifactPayload =
    $(embedFile "test/fixtures/truffle/Hellos.json")
