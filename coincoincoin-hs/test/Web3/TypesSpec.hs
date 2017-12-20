{-# LANGUAGE TemplateHaskell #-}

module Web3.TypesSpec (main, spec) where

import qualified Data.Aeson      as AE
import Data.ByteString.Char8 (ByteString)
import Data.FileEmbed  (embedFile)
import Test.Hspec

import Web3.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Event's FromJSON instance" $
        it "decodes Event JSON" $ do
            let (Right event) = AE.eitherDecodeStrict votedEventPayload :: Either String (Event Voted VotedReturnValues)
            eventEvent event `shouldBe` Voted
            eventReturnValues event `shouldBe` expectedReturnValues

votedEventPayload :: ByteString
votedEventPayload =
    $(embedFile "test/fixtures/eth/events/voted.json")

expectedReturnValues :: VotedReturnValues
expectedReturnValues =
    VotedReturnValues
        { proposalID = "1"
        , position = True
        , voter = Address "0xb3ED286C1D088016589B5d2B0729A73A1e24f8A7"
        , justification = "Gimme!"
        }
