module Congress.Events.Processor
    ( processEvent
    ) where

import           Control.Monad (void)
import           Control.Monad.Logger (MonadLogger, logInfoN)
import qualified Data.Text as T

import CoinCoinCoin.Class (MonadTime(..))
import CoinCoinCoin.Congress.Events.Types
import CoinCoinCoin.Database.Models (CongressMembership(..))
import Web3.Types (Event(..))

import App (MonadDbWriter(..))

processEvent :: ( MonadLogger m
                , MonadDbWriter m
                , MonadTime m
                ) => CongressEvent -> m ()
processEvent e@(ChangeOfRulesEvt _) = logInfoN . T.pack $ show e
processEvent e@(ProposalAddedEvt _) = logInfoN . T.pack $ show e
processEvent e@(ProposalTalliedEvt _) = logInfoN . T.pack $ show e
processEvent e@(ReceivedEtherEvt _) = logInfoN . T.pack $ show e
processEvent e@(VotedEvt _) = logInfoN . T.pack $ show e
processEvent e@(MembershipChangedEvt evt) = do
    logInfoN . T.pack $ show e
    processMembershipChanged evt

processMembershipChanged :: ( MonadLogger m
                            , MonadDbWriter m
                            , MonadTime m
                            ) => Event MembershipChanged MembershipChangedValues -> m ()
processMembershipChanged evt = do
    logInfoN . T.pack $ show evt
    now <- getCurrentTime
    void . upsertCongressMembership $ membership now
    where
        membership now = CongressMembership (member val) (isMember val) now now
        val = eventReturnValues evt
