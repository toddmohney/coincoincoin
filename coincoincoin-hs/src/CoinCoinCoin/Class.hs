{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module CoinCoinCoin.Class
    ( MonadDb
    , MonadDbReader(..)
    , MonadDbWriter(..)
    , MonadFileReader(..)
    , MonadTime(..)
    , UTCTime
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time
import qualified System.Directory as D

import CoinCoinCoin.Database.Models
    ( Address
    , Entity
    , CongressMembership
    , CongressMembershipId
    , KafkaClientId
    , KafkaOffset
    , KafkaOffsetId
    , Partition
    , TopicName
    )

class (Monad m) => MonadTime m where
    getCurrentTime :: m UTCTime

instance MonadTime IO where
    getCurrentTime = Time.getCurrentTime

type MonadDb m = (MonadDbReader m, MonadDbWriter m)

class (Monad m) => MonadDbReader m where
    type DbReaderType m :: * -> *

    runDbReader :: (DbReaderType m) a -> m a

    getCongressMembership :: Address -> m (Maybe (Entity CongressMembership))

    getAllKafkaOffsets :: m [Entity KafkaOffset]

    getKafkaOffset :: KafkaClientId -> TopicName -> Partition -> m (Maybe (Entity KafkaOffset))

class (Monad m) => MonadDbWriter m where
    type DbWriterType m :: * -> *

    runDbWriter :: (DbWriterType m) a -> m a

    upsertCongressMembership :: CongressMembership -> m CongressMembershipId

    createKafkaOffset :: KafkaOffset -> m KafkaOffsetId

    updateKafkaOffset :: KafkaOffsetId -> KafkaOffset -> m ()

    incrementKafkaOffset :: KafkaOffset -> m ()

class (Monad m) => MonadFileReader m where
    doesDirectoryExist :: FilePath -> m Bool

    listDirectory :: FilePath -> m [FilePath]

    readFile :: FilePath -> m ByteString

instance MonadFileReader IO where
    doesDirectoryExist :: FilePath -> IO Bool
    doesDirectoryExist = D.doesDirectoryExist

    listDirectory :: FilePath -> IO [FilePath]
    listDirectory = D.listDirectory

    readFile :: FilePath -> IO ByteString
    readFile = BS.readFile
