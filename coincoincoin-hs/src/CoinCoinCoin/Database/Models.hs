{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module CoinCoinCoin.Database.Models
    ( module CoinCoinCoin.Database.Models
    , Address(..)
    , ConnectionPool
    , ConnectionString
    , Entity(..)
    , Partition
    , SqlPersistT
    , TopicName
    , toSqlKey
    , fromSqlKey
    ) where

import           Data.ByteString (ByteString)
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime)
import           Database.Persist.Postgresql
    ( ConnectionPool
    , ConnectionString
    , Entity(..)
    , PersistField(..)
    , PersistValue(..)
    , SqlPersistT
    , runMigration
    , runMigrationSilent
    , runSqlPool
    )
import           Database.Persist.Sql
    ( Key(..)
    , SqlBackend
    , ToBackendKey
    , fromSqlKey
    , toSqlKey
    )
import           Database.Persist.TH
import           GHC.Generics (Generic)
import           Network.Kafka.Protocol
    ( KafkaString(..)
    , Offset(..)
    , Partition(..)
    , TopicName(..)
    )

import qualified Truffle.Types as T
import Web3.Types (Address(..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    CongressMembership sql=congress_memberships
        member Address sqltype=text
        isMember Bool sqltype=boolean
        created UTCTime default=now()
        updated UTCTime default=now()
        UniqueMemberAddress member
        deriving Show Eq Generic

    Contract sql=contracts
        name Text sqltype=text
        networkId T.NetworkId sqltype=text
        address Address sqltype=text
        abi ByteString sqltype=bytea
        updatedAt UTCTime default=now()
        UniqueContractAddressAndNetwork address networkId
        deriving Show Eq Generic

    KafkaOffset sql=kafka_offsets
        topic TopicName sqltype=text
        partition Partition sqltype=int
        offset Offset sqltype=bigint
        clientId KafkaClientId sqltype=text
        created UTCTime default=now()
        updated UTCTime default=now()
        UniqueKafkaOffsetsTopicPartition topic partition clientId
        deriving Show Eq Generic
|]

newtype KafkaClientId = KafkaClientId Text
    deriving (Show, Eq, IsString)

instance PersistField T.NetworkId where
  toPersistValue (T.NetworkId networkId) = PersistText networkId
  fromPersistValue (PersistText networkId) = Right (T.NetworkId networkId)
  fromPersistValue networkId = Left ("Not PersistText " <> T.pack (show networkId))

instance PersistField Address where
  toPersistValue (Address addr) = PersistText addr
  fromPersistValue (PersistText addr) = Right (Address addr)
  fromPersistValue addr = Left ("Not PersistText " <> T.pack (show addr))

instance PersistField Offset where
  toPersistValue = PersistInt64 . fromIntegral
  fromPersistValue (PersistInt64 offset) = Right (Offset offset)
  fromPersistValue offset = Left ("Not PersistInt64 " <> T.pack (show offset))

instance PersistField KafkaClientId where
  toPersistValue (KafkaClientId clientId) = PersistText clientId
  fromPersistValue (PersistText clientId) = Right (KafkaClientId clientId)
  fromPersistValue clientId = Left ("Not PersistText " <> T.pack (show clientId))

instance PersistField Partition where
  toPersistValue = PersistInt64 . fromIntegral
  fromPersistValue (PersistInt64 partition) = Right (Partition $ fromIntegral partition)
  fromPersistValue partition = Left ("Not PersistInt64 " <> T.pack (show partition))

instance PersistField TopicName where
  toPersistValue (TName (KString topicName)) = PersistDbSpecific topicName
  fromPersistValue (PersistText topicName) = Right (TName . KString $ T.encodeUtf8 topicName)
  fromPersistValue topic = Left ("Not PersistText " <> T.pack (show topic))

runMigrations :: ConnectionPool -> IO ()
runMigrations =
    runSqlPool (runMigration migrateAll)

runMigrations' :: ConnectionPool -> IO [Text]
runMigrations' =
    runSqlPool (runMigrationSilent migrateAll)

toKey :: (Integral i, ToBackendKey SqlBackend record) => i -> Key record
toKey = toSqlKey . fromIntegral

