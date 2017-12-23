module CoinCoinCoin.Database.CongressMemberships.Query
    ( getCongressMembership
    , createCongressMembership
    , updateCongressMembership
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Postgresql

import CoinCoinCoin.Database.Models

getCongressMembership :: Address -> SqlPersistT IO (Maybe (Entity CongressMembership))
getCongressMembership addr =
    getBy (UniqueMemberAddress addr)

createCongressMembership :: CongressMembership -> SqlPersistT IO CongressMembershipId
createCongressMembership = insert

updateCongressMembership :: CongressMembershipId -> CongressMembership -> SqlPersistT IO ()
updateCongressMembership membershipId membership = do
    now <- liftIO getCurrentTime
    updateWhere [ CongressMembershipId ==. membershipId ]
        [ CongressMembershipIsMember =. congressMembershipIsMember membership
        , CongressMembershipUpdated =. now
        ]


