module CoinCoinCoin.Database.KafkaOffsets.QuerySpec (main, spec) where

import qualified Data.Time.Clock as C
import qualified Database.Persist.Postgresql as Sql
import           Test.Hspec

import Helpers.DatabaseHelpers

import qualified CoinCoinCoin.Database.KafkaOffsets.Query as Q
import           CoinCoinCoin.Database.Models

main :: IO ()
main = hspec spec

spec :: Spec
spec = before prepDb $
    describe "incrementKafkaOffset" $ do
        it "creates a new record when there is no unique key match" $ \dbPool -> do
            now <- C.getCurrentTime
            let offset = mkOffset now

            (Entity oId o) <- Sql.runSqlPool (Q.incrementKafkaOffset offset) dbPool
            oId `shouldBe` toKey (1 :: Int)
            kafkaOffsetOffset o `shouldBe` 1

        it "increments the offset of an existing record when there is a unique key match" $ \dbPool -> do
            now <- C.getCurrentTime
            let offset = mkOffset now

            (Entity oId1 o1) <- Sql.runSqlPool (Q.incrementKafkaOffset offset) dbPool
            oId1 `shouldBe` toKey (1 :: Int)
            kafkaOffsetOffset o1 `shouldBe` 1

            (Entity oId2 o2) <- Sql.runSqlPool (Q.incrementKafkaOffset offset) dbPool
            oId2 `shouldBe` toKey (1 :: Int)
            kafkaOffsetOffset o2 `shouldBe` 2

            (Entity oId3 o3) <- Sql.runSqlPool (Q.incrementKafkaOffset offset) dbPool
            oId3 `shouldBe` toKey (1 :: Int)
            kafkaOffsetOffset o3 `shouldBe` 3

mkOffset :: C.UTCTime -> KafkaOffset
mkOffset now =
    KafkaOffset
        { kafkaOffsetTopic = "some-topic"
        , kafkaOffsetPartition = 1
        , kafkaOffsetOffset = 1
        , kafkaOffsetClientId = "some-client"
        , kafkaOffsetCreated = now
        , kafkaOffsetUpdated = now
        }
