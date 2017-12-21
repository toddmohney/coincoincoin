{-# LANGUAGE TemplateHaskell #-}

module Web3.TypesSpec (main, spec) where

import qualified Data.Aeson      as AE
import Data.ByteString.Char8 (ByteString)
import Data.FileEmbed  (embedFile)
import Test.Hspec

import CoinCoinCoin.Congress.Events.Types (CongressEvent(..), VotedReturnValues(..))
import Web3.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Event's FromJSON instance" $
        it "decodes Event JSON" $ do
            let (Right e) = AE.eitherDecodeStrict votedEventPayload :: Either String (Event CongressEvent VotedReturnValues)
            eventAddress e `shouldBe` "0x321d5513c291De3a2fB63bf4B6E711c34F57bA28"
            eventBlockNumber e `shouldBe` 11712
            eventTransactionHash e `shouldBe` "0x36efe9ee9d80244a662b115fa210b9cf15a056dcc48eff1097bdf8d5399ae88d"
            eventTransactionIndex e `shouldBe` 0
            eventBlockHash e `shouldBe` "0x694a20f7da2ddb88889abb159c4bc0bbfdddd461ede8b90800e838d147e6a323"
            eventLogIndex e `shouldBe` 0
            eventRemoved e `shouldBe` False
            eventId e `shouldBe` "log_1d35456e"
            eventReturnValues e `shouldBe` expectedReturnValues
            eventEvent e `shouldBe` Voted
            eventSignature e `shouldBe` "0xc34f869b7ff431b034b7b9aea9822dac189a685e0b015c7d1be3add3f89128e8"

votedEventPayload :: ByteString
votedEventPayload =
    $(embedFile "test/fixtures/eth/events/voted.json")

expectedReturnValues :: VotedReturnValues
expectedReturnValues =
    VotedReturnValues
        { proposalID = 1
        , position = True
        , voter = Address "0xb3ED286C1D088016589B5d2B0729A73A1e24f8A7"
        , justification = "Gimme!"
        }
