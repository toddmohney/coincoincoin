module CoinCoinCoin.StatsD
    ( Bucket(..)
    , Metric(..)
    , Stat(..)
    , StatsDConfig(..)
    , send
    , time
    , withStatsD
    , withStatsDTimed
    ) where

import           Control.Exception (bracket)
import           Control.Monad.Reader
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import qualified Data.Text.Lazy as LT
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Network.Socket (HostName, ServiceName)
import qualified Network.StatsD as SD
import           Numeric.Natural

data StatsDConfig = StatsDConfig
    { hostName    :: HostName
    , serviceName :: ServiceName
    } deriving (Show, Eq)

newtype Bucket = Bucket LT.Text
    deriving (Show, Eq, IsString)

newtype StatsDT m a = StatsDT { unStatsDT :: ReaderT SD.StatsD m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader SD.StatsD
             )

data Stat = Stat
    { bucket :: Bucket
    , metric :: Metric
    } deriving (Show, Eq)

data Metric = Counter Natural
            -- ^ A monotonically increasing measurement
            | Gauge Float
            -- ^ An arbitrary measurement
            | Delta Float
            -- ^ An incremental modification to an existing Gauge measurement
            | Timing Natural
            -- ^ A measurement of time-elapsed in milliseconds
            | Histogram Natural
            -- ^ A measurement of time-elapsed in milliseconds
            | Set Int
            -- ^ A measurement for tracking unique occurences
    deriving (Show, Eq)

runStatsDT :: SD.StatsD -> StatsDT m a -> m a
runStatsDT statsD statsDT = runReaderT (unStatsDT statsDT) statsD

withStatsD :: StatsDConfig -> StatsDT IO a -> IO a
withStatsD statsDCfg f =
    bracket (connectToStatsD statsDCfg) disconnectFromStatsD $ \statsD ->
        runStatsDT statsD f

{- |
 - StatsDConfig - configuration options
 - Bucket - the bucket for the timing metrics
 - StatsDT IO a - the measured computation within the timing
 -}
withStatsDTimed :: StatsDConfig -> Bucket -> StatsDT IO a -> IO a
withStatsDTimed statsDCfg bucket f =
    withStatsD statsDCfg $
        time bucket (withStatsD statsDCfg f)

connectToStatsD :: StatsDConfig -> IO SD.StatsD
connectToStatsD (StatsDConfig host port) = SD.openStatsD host port ["app","test"]

disconnectFromStatsD :: SD.StatsD -> IO ()
disconnectFromStatsD = SD.closeStatsD

send :: [Stat] -> StatsDT IO ()
send stats =
    ask >>= \statsD ->
        liftIO $ SD.push statsD (map toStat stats)

toStat :: Stat -> SD.Stat
toStat (Stat (Bucket bucket) (Counter ctr)) = SD.Stat bucket (LT.pack (show ctr)) "c" Nothing
toStat (Stat (Bucket bucket) (Gauge g))     = SD.Stat bucket (LT.pack (show g)) "g" Nothing
toStat (Stat (Bucket bucket) (Timing t))    = SD.Stat bucket (LT.pack (show t)) "ms" Nothing
toStat (Stat (Bucket bucket) (Histogram h)) = SD.Stat bucket (LT.pack (show h)) "h" Nothing
toStat (Stat (Bucket bucket) (Set s))       = SD.Stat bucket (LT.pack (show s)) "s" Nothing
toStat (Stat (Bucket bucket) (Delta d))     =
    if d >= 0
        then
            SD.Stat bucket (LT.pack ("+" <> show (abs d))) "g" Nothing
        else
            SD.Stat bucket (LT.pack ("-" <> show (abs d))) "g" Nothing

time :: Bucket -> IO a -> StatsDT IO a
time bucket f = do
    start <- liftIO getCurrentTime
    result <- liftIO f
    finish <- liftIO getCurrentTime

    -- round to nearest millisecond and send to StatsD
    send [Stat bucket (Timing $ round (diffUTCTime finish start * 1000))]

    return result
