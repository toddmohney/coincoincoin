module CoinCoinCoin.Database.CongressMemberships.Query
    ( getCongressMembership
    , createCongressMembership
    , updateCongressMembershipStatus
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

updateCongressMembershipStatus :: CongressMembershipId -> CongressMembership -> SqlPersistT IO ()
updateCongressMembershipStatus membershipId membership = do
    now <- liftIO getCurrentTime
    updateWhere [ CongressMembershipId ==. membershipId ]
        [ CongressMembershipIsMember =. congressMembershipIsMember membership
        , CongressMembershipUpdated =. now
        ]


