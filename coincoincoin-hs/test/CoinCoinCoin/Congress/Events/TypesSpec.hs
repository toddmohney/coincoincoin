{-# LANGUAGE TemplateHaskell #-}

module CoinCoinCoin.Congress.Events.TypesSpec (main, spec) where

import qualified Data.Aeson as AE
import           Data.ByteString.Char8 (ByteString)
import           Data.FileEmbed (embedFile)
import           Test.Hspec

import CoinCoinCoin.Congress.Events.Types
import Web3.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "ChangeOfRules event and return value" $
        it "implements FromJSON" $ do
            let (Right e) = AE.eitherDecodeStrict changeOfRulesEventPayload :: Either String (Event CongressEvent ChangeOfRulesReturnValues)
            eventReturnValues e `shouldBe` expectedChangeOfRulesReturnValues

    describe "MembershipChanged event and return value" $
        it "implements FromJSON" $ do
            let (Right e) = AE.eitherDecodeStrict membershipChangedEventPayload :: Either String (Event CongressEvent MembershipChangedReturnValues)
            eventReturnValues e `shouldBe` expectedMembershipChangedReturnValues
            eventEvent e `shouldBe` MembershipChanged

    describe "ProposalAdded event and return value" $
        it "implements FromJSON" $ do
            let (Right e) = AE.eitherDecodeStrict proposalAddedEventPayload :: Either String (Event CongressEvent ProposalAddedReturnValues)
            eventReturnValues e `shouldBe` expectedProposalAddedReturnValues
            eventEvent e `shouldBe` ProposalAdded

    describe "ProposalTallied event and return value" $
        it "implements FromJSON" $ do
            let (Right e) = AE.eitherDecodeStrict proposalTalliedEventPayload :: Either String (Event CongressEvent ProposalTalliedReturnValues)
            eventReturnValues e `shouldBe` expectedProposalTalliedReturnValues
            eventEvent e `shouldBe` ProposalTallied

    describe "ReceivedEther event and return value" $
        it "implements FromJSON" $ do
            let (Right e) = AE.eitherDecodeStrict receivedEtherEventPayload :: Either String (Event CongressEvent ReceivedEtherReturnValues)
            eventReturnValues e `shouldBe` expectedReceivedEtherReturnValues
            eventEvent e `shouldBe` ReceivedEther

    describe "Voted event and return value" $
        it "implements FromJSON" $ do
            let (Right e) = AE.eitherDecodeStrict votedEventPayload :: Either String (Event CongressEvent VotedReturnValues)
            eventReturnValues e `shouldBe` expectedVotedReturnValues
            eventEvent e `shouldBe` Voted

changeOfRulesEventPayload :: ByteString
changeOfRulesEventPayload =
    $(embedFile "test/fixtures/eth/events/change-of-rules.json")

membershipChangedEventPayload :: ByteString
membershipChangedEventPayload =
    $(embedFile "test/fixtures/eth/events/membership-changed.json")

proposalAddedEventPayload :: ByteString
proposalAddedEventPayload =
    $(embedFile "test/fixtures/eth/events/proposal-added.json")

proposalTalliedEventPayload :: ByteString
proposalTalliedEventPayload =
    $(embedFile "test/fixtures/eth/events/proposal-tallied.json")

receivedEtherEventPayload :: ByteString
receivedEtherEventPayload =
    $(embedFile "test/fixtures/eth/events/received-ether.json")

votedEventPayload :: ByteString
votedEventPayload =
    $(embedFile "test/fixtures/eth/events/voted.json")

expectedVotedReturnValues :: VotedReturnValues
expectedVotedReturnValues =
    VotedReturnValues
        { proposalID = 1
        , position = True
        , voter = Address "0xb3ED286C1D088016589B5d2B0729A73A1e24f8A7"
        , justification = "Gimme!"
        }

expectedMembershipChangedReturnValues :: MembershipChangedReturnValues
expectedMembershipChangedReturnValues =
    MembershipChangedReturnValues
        { member = "0x0000000000000000000000000000000000000000"
        , isMember = True
        }

expectedChangeOfRulesReturnValues :: ChangeOfRulesReturnValues
expectedChangeOfRulesReturnValues =
    ChangeOfRulesReturnValues 0 0 0

expectedProposalAddedReturnValues :: ProposalAddedReturnValues
expectedProposalAddedReturnValues =
    ProposalAddedReturnValues
        { proposalAddedProposalID = 0
        , proposalAddedRecipient = "0xb3ED286C1D088016589B5d2B0729A73A1e24f8A7"
        , proposalAddedAmount = 300000000000000000000
        , proposalAddedDescription = "Cuz expensive"
        }

expectedProposalTalliedReturnValues :: ProposalTalliedReturnValues
expectedProposalTalliedReturnValues =
    ProposalTalliedReturnValues
        { proposalTalliedProposalID = 1
        , proposalTalliedResult = 1
        , proposalTalliedQuorum = 1
        , proposalTalliedActive = True
        }

expectedReceivedEtherReturnValues :: ReceivedEtherReturnValues
expectedReceivedEtherReturnValues =
    ReceivedEtherReturnValues
        { receivedEtherSender = "0xb3ED286C1D088016589B5d2B0729A73A1e24f8A7"
        , receivedEtherAmount = 200000000000000000000
        }
