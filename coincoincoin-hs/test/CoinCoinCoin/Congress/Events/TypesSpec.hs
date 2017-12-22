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
            let (Right (ChangeOfRulesEvt e)) = AE.eitherDecodeStrict changeOfRulesEventPayload :: Either String CongressEvent
            eventReturnValues e `shouldBe` expectedChangeOfRulesReturnValues
            eventEvent e `shouldBe` ChangeOfRules

    describe "MembershipChanged event and return value" $
        it "implements FromJSON" $ do
            let (Right (MembershipChangedEvt e)) = AE.eitherDecodeStrict membershipChangedEventPayload :: Either String CongressEvent
            eventReturnValues e `shouldBe` expectedMembershipChangedReturnValues
            eventEvent e `shouldBe` MembershipChanged

    describe "ProposalAdded event and return value" $
        it "implements FromJSON" $ do
            let (Right (ProposalAddedEvt e)) = AE.eitherDecodeStrict proposalAddedEventPayload :: Either String CongressEvent
            eventReturnValues e `shouldBe` expectedProposalAddedReturnValues
            eventEvent e `shouldBe` ProposalAdded

    describe "ProposalTallied event and return value" $
        it "implements FromJSON" $ do
            let (Right (ProposalTalliedEvt e)) = AE.eitherDecodeStrict proposalTalliedEventPayload :: Either String CongressEvent
            eventReturnValues e `shouldBe` expectedProposalTalliedReturnValues
            eventEvent e `shouldBe` ProposalTallied

    describe "ReceivedEther event and return value" $
        it "implements FromJSON" $ do
            let (Right (ReceivedEtherEvt e)) = AE.eitherDecodeStrict receivedEtherEventPayload :: Either String CongressEvent
            eventReturnValues e `shouldBe` expectedReceivedEtherReturnValues
            eventEvent e `shouldBe` ReceivedEther

    describe "Voted event and return value" $
        it "implements FromJSON" $ do
            let (Right (VotedEvt e)) = AE.eitherDecodeStrict votedEventPayload :: Either String CongressEvent
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

expectedVotedReturnValues :: VotedValues
expectedVotedReturnValues =
    VotedValues
        { proposalID = 1
        , position = True
        , voter = Address "0xb3ED286C1D088016589B5d2B0729A73A1e24f8A7"
        , justification = "Gimme!"
        }

expectedMembershipChangedReturnValues :: MembershipChangedValues
expectedMembershipChangedReturnValues =
    MembershipChangedValues
        { member = "0x0000000000000000000000000000000000000000"
        , isMember = True
        }

expectedChangeOfRulesReturnValues :: ChangeOfRulesValues
expectedChangeOfRulesReturnValues =
    ChangeOfRulesValues 0 0 0

expectedProposalAddedReturnValues :: ProposalAddedValues
expectedProposalAddedReturnValues =
    ProposalAddedValues
        { proposalAddedProposalID = 0
        , proposalAddedRecipient = "0xb3ED286C1D088016589B5d2B0729A73A1e24f8A7"
        , proposalAddedAmount = 300000000000000000000
        , proposalAddedDescription = "Cuz expensive"
        }

expectedProposalTalliedReturnValues :: ProposalTalliedValues
expectedProposalTalliedReturnValues =
    ProposalTalliedValues
        { proposalTalliedProposalID = 1
        , proposalTalliedResult = 1
        , proposalTalliedQuorum = 1
        , proposalTalliedActive = True
        }

expectedReceivedEtherReturnValues :: ReceivedEtherValues
expectedReceivedEtherReturnValues =
    ReceivedEtherValues
        { receivedEtherSender = "0xb3ED286C1D088016589B5d2B0729A73A1e24f8A7"
        , receivedEtherAmount = 200000000000000000000
        }
