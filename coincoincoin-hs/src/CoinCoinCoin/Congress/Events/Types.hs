module CoinCoinCoin.Congress.Events.Types
    ( CongressEvent(..)
    , ChangeOfRulesReturnValues(..)
    , MembershipChangedReturnValues(..)
    , ProposalAddedReturnValues(..)
    , ProposalTalliedReturnValues(..)
    , ReceivedEtherReturnValues(..)
    , VotedReturnValues(..)
    ) where

import           Control.Applicative ((<|>))
import           Data.Aeson (FromJSON(..), Object, Value(..), (.:))
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson.Types as AE
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           GHC.Generics (Generic)
import           Text.Read (readMaybe)

import Web3.Types (Address(..))

data CongressEvent = Voted
                   | ChangeOfRules
                   | MembershipChanged
                   | ProposalAdded
                   | ProposalTallied
                   | ReceivedEther
    deriving (Show, Eq, Generic)

instance FromJSON CongressEvent where
    parseJSON str@(String s) =
        case TL.unpack (TL.fromStrict s) of
            "Voted" -> pure Voted
            "ChangeOfRules" -> pure ChangeOfRules
            "MembershipChanged" -> pure MembershipChanged
            "ProposalAdded" -> pure ProposalAdded
            "ProposalTallied" -> pure ProposalTallied
            "receivedEther" -> pure ReceivedEther
            _  -> AE.typeMismatch "Unable to parse as Voted: " str
    parseJSON v = AE.typeMismatch "Unable to parse as Voted" v


data ChangeOfRulesReturnValues = ChangeOfRulesReturnValues
    { newMinimumQuorum           :: Int
    , newDebatingPeriodInMinutes :: Int
    , newMajorityMargin          :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON ChangeOfRulesReturnValues where
    parseJSON = AE.withObject "Event" $ \o ->
        ChangeOfRulesReturnValues
            <$> flexibleNumParser o "newMinimumQuorum"
            <*> flexibleNumParser o "newDebatingPeriodInMinutes"
            <*> flexibleNumParser o "newMajorityMargin"


data MembershipChangedReturnValues = MembershipChangedReturnValues
    { member   :: Address
    , isMember :: Bool
    } deriving (Show, Eq, Generic)

instance FromJSON MembershipChangedReturnValues


data ProposalAddedReturnValues = ProposalAddedReturnValues
    { proposalAddedProposalID  :: Int
    , proposalAddedRecipient   :: Address
    , proposalAddedAmount      :: Integer
    , proposalAddedDescription :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON ProposalAddedReturnValues where
    parseJSON = AE.withObject "Event" $ \o ->
        ProposalAddedReturnValues
            <$> flexibleNumParser o "proposalID"
            <*> o .: "recipient"
            <*> flexibleNumParser o "amount"
            <*> o .: "description"


data ProposalTalliedReturnValues = ProposalTalliedReturnValues
    { proposalTalliedProposalID :: Int
    , proposalTalliedResult     :: Int
    , proposalTalliedQuorum     :: Int
    , proposalTalliedActive     :: Bool
    } deriving (Show, Eq, Generic)

instance FromJSON ProposalTalliedReturnValues where
    parseJSON = AE.withObject "Event" $ \o ->
        ProposalTalliedReturnValues
            <$> flexibleNumParser o "proposalID"
            <*> flexibleNumParser o "result"
            <*> flexibleNumParser o "quorum"
            <*> o .: "active"


data ReceivedEtherReturnValues = ReceivedEtherReturnValues
    { receivedEtherSender :: Address
    , receivedEtherAmount :: Integer
    } deriving (Show, Eq, Generic)

instance FromJSON ReceivedEtherReturnValues where
    parseJSON = AE.withObject "Event" $ \o ->
        ReceivedEtherReturnValues
            <$> o .: "sender"
            <*> flexibleNumParser o "amount"


data VotedReturnValues = VotedReturnValues
    { proposalID    :: Int
    , position      :: Bool
    , voter         :: Address
    , justification :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON VotedReturnValues where
    parseJSON = AE.withObject "Event" $ \o ->
        VotedReturnValues
            <$> flexibleNumParser o "proposalID"
            <*> o .: "position"
            <*> o .: "voter"
            <*> o .: "justification"


flexibleNumParser :: ( FromJSON a
                     , Num a
                     , Read a
                     ) => Object -> Text -> Parser a
flexibleNumParser o key =
    o .: key <|> (o .: key >>= stringToNum)


stringToNum :: ( FromJSON a
               , Num a
               , Read a
               ) => String -> Parser a
stringToNum s =
    case readMaybe s of
        Nothing -> AE.typeMismatch "Unable to parse as Num:" (String (T.pack s))
        (Just i) -> return i
