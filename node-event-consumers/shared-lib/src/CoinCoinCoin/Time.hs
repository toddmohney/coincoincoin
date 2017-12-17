module CoinCoinCoin.Time
    ( aWeekAgo
    ) where

import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (getCurrentTime, posixDayLength)

aWeekAgo :: IO UTCTime
aWeekAgo =
    addUTCTime ((-7) * posixDayLength) <$> getCurrentTime
