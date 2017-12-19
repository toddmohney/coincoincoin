{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module CoinCoinCoin.Database.Models
    ( module CoinCoinCoin.Database.Models
    , ConnectionPool
    , Entity(..)
    , Partition
    , SqlPersistT
    , TopicName
    , toSqlKey
    , fromSqlKey
    ) where

import           Data.Monoid                 ((<>))
import           Data.String                 (IsString (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Time.Clock             (UTCTime)
import           Data.Typeable               (Typeable)
import           Database.Persist.Postgresql
    ( ConnectionPool
    , Entity (..)
    , PersistField (..)
    , PersistValue (..)
    , SqlPersistT
    , runMigration
    , runSqlPool
    )
import           Database.Persist.Sql
    ( Key (..)
    , SqlBackend
    , ToBackendKey
    , fromSqlKey
    , toSqlKey
    )
import           Database.Persist.TH
import           GHC.Generics                (Generic)
import           Network.Kafka.Protocol
    ( KafkaString (..)
    , Offset (..)
    , Partition (..)
    , TopicName (..)
    )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    KafkaOffset sql=kafka_offsets
        topic TopicName sqltype=text
        partition Partition sqltype=int
        offset Offset sqltype=bigint
        clientId KafkaClientId sqltype=text
        created UTCTime default=now()
        updated UTCTime default=now()
        UniqueKafkaOffsetsTopicPartition topic partition clientId
        deriving Show Eq Typeable Generic
|]

newtype KafkaClientId = KafkaClientId Text
    deriving (Show, Eq, IsString)

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

toKey :: (Integral i, ToBackendKey SqlBackend record) => i -> Key record
toKey = toSqlKey . fromIntegral

