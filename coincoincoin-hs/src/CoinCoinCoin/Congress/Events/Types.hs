module CoinCoinCoin.Congress.Events.Types
    ( CongressEvent(..)
    , ChangeOfRules(..)
    , ChangeOfRulesValues(..)
    , MembershipChanged(..)
    , MembershipChangedValues(..)
    , ProposalAdded(..)
    , ProposalAddedValues(..)
    , ProposalTallied(..)
    , ProposalTalliedValues(..)
    , ReceivedEther(..)
    , ReceivedEtherValues(..)
    , Voted(..)
    , VotedValues(..)
    ) where

import           Control.Applicative ((<|>))
import           Data.Aeson (FromJSON(..), Object, Value(..), (.:))
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson.Types as AE
import           Data.Foldable (asum)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Text.Read (readMaybe)

import Web3.Types (Address(..), Event(..))

data CongressEvent = ChangeOfRulesEvt (Event ChangeOfRules ChangeOfRulesValues)
                   | MembershipChangedEvt (Event MembershipChanged MembershipChangedValues)
                   | ProposalAddedEvt (Event ProposalAdded ProposalAddedValues)
                   | ProposalTalliedEvt (Event ProposalTallied ProposalTalliedValues)
                   | ReceivedEtherEvt (Event ReceivedEther ReceivedEtherValues)
                   | VotedEvt (Event Voted VotedValues)
    deriving (Show)

instance FromJSON CongressEvent where
    parseJSON val = asum
        [ ChangeOfRulesEvt <$> parseJSON val
        , MembershipChangedEvt <$> parseJSON val
        , ProposalAddedEvt <$> parseJSON val
        , ProposalTalliedEvt <$> parseJSON val
        , ReceivedEtherEvt <$> parseJSON val
        , VotedEvt <$> parseJSON val
        ]

data ChangeOfRules = ChangeOfRules
    deriving (Show, Eq, Generic)

instance FromJSON ChangeOfRules where
    parseJSON val = parseEventType val "ChangeOfRules" ChangeOfRules

data MembershipChanged = MembershipChanged
    deriving (Show, Eq, Generic)

instance FromJSON MembershipChanged where
    parseJSON val = parseEventType val "MembershipChanged" MembershipChanged

data ProposalAdded = ProposalAdded
    deriving (Show, Eq, Generic)

instance FromJSON ProposalAdded where
    parseJSON val = parseEventType val "ProposalAdded" ProposalAdded

data ProposalTallied = ProposalTallied
    deriving (Show, Eq, Generic)

instance FromJSON ProposalTallied where
    parseJSON val = parseEventType val "ProposalTallied" ProposalTallied

data ReceivedEther = ReceivedEther
    deriving (Show, Eq, Generic)

instance FromJSON ReceivedEther where
    parseJSON val = parseEventType val "receivedEther" ReceivedEther

data Voted = Voted
    deriving (Show, Eq, Generic)

instance FromJSON Voted where
    parseJSON val = parseEventType val "Voted" Voted

data ChangeOfRulesValues = ChangeOfRulesValues
    { newMinimumQuorum           :: Int
    , newDebatingPeriodInMinutes :: Int
    , newMajorityMargin          :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON ChangeOfRulesValues where
    parseJSON = AE.withObject "Event" $ \o ->
        ChangeOfRulesValues
            <$> flexibleNumParser o "newMinimumQuorum"
            <*> flexibleNumParser o "newDebatingPeriodInMinutes"
            <*> flexibleNumParser o "newMajorityMargin"


data MembershipChangedValues = MembershipChangedValues
    { member   :: Address
    , isMember :: Bool
    } deriving (Show, Eq, Generic)

instance FromJSON MembershipChangedValues


data ProposalAddedValues = ProposalAddedValues
    { proposalAddedProposalID  :: Int
    , proposalAddedRecipient   :: Address
    , proposalAddedAmount      :: Integer
    , proposalAddedDescription :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON ProposalAddedValues where
    parseJSON = AE.withObject "Event" $ \o ->
        ProposalAddedValues
            <$> flexibleNumParser o "proposalID"
            <*> o .: "recipient"
            <*> flexibleNumParser o "amount"
            <*> o .: "description"


data ProposalTalliedValues = ProposalTalliedValues
    { proposalTalliedProposalID :: Int
    , proposalTalliedResult     :: Int
    , proposalTalliedQuorum     :: Int
    , proposalTalliedActive     :: Bool
    } deriving (Show, Eq, Generic)

instance FromJSON ProposalTalliedValues where
    parseJSON = AE.withObject "Event" $ \o ->
        ProposalTalliedValues
            <$> flexibleNumParser o "proposalID"
            <*> flexibleNumParser o "result"
            <*> flexibleNumParser o "quorum"
            <*> o .: "active"


data ReceivedEtherValues = ReceivedEtherValues
    { receivedEtherSender :: Address
    , receivedEtherAmount :: Integer
    } deriving (Show, Eq, Generic)

instance FromJSON ReceivedEtherValues where
    parseJSON = AE.withObject "Event" $ \o ->
        ReceivedEtherValues
            <$> o .: "sender"
            <*> flexibleNumParser o "amount"


data VotedValues = VotedValues
    { proposalID    :: Int
    , position      :: Bool
    , voter         :: Address
    , justification :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON VotedValues where
    parseJSON = AE.withObject "Event" $ \o ->
        VotedValues
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

parseEventType :: (Show a) => Value -> Text -> a -> Parser a
parseEventType str@(String s) key typ =
    if s == key
        then pure typ
        else AE.typeMismatch "Unable to parse as Voted: " str
parseEventType v _ typ =
    AE.typeMismatch ("Unable to parse as " <> show typ) v
