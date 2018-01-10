module CoinCoinCoin.Database.Contracts.QuerySpec (main, spec) where

import qualified Data.Time.Clock as C
import qualified Database.Persist.Postgresql as Sql
import           Test.Hspec

import Helpers.DatabaseHelpers

import qualified CoinCoinCoin.Database.Contracts.Query as Q
import           CoinCoinCoin.Database.Models

main :: IO ()
main = hspec spec

spec :: Spec
spec = before prepDb $
    describe "upsertContract" $ do
        it "inserts a new record when a record matching the unique key is not found" $ \dbPool -> do
            now <- C.getCurrentTime
            let contract = Contract "SomeContract" "some-network-id" "0x1234" "some-abi" now
            (Entity cId c) <- Sql.runSqlPool (Q.upsertContract contract) dbPool
            cId `shouldBe` toKey (1 :: Int)
            contractAddress c `shouldBe` Address "0x1234"

        it "updates an existing record matching the unique key" $ \dbPool -> do
            now1 <- C.getCurrentTime
            let contract1 = Contract "SomeContract" "some-network-id" "0x1234" "some-abi" now1

            (Entity cId1 c1) <- Sql.runSqlPool (Q.upsertContract contract1) dbPool
            cId1 `shouldBe` toKey (1 :: Int)
            contractAbi c1 `shouldBe` "some-abi"
            contractUpdatedAt c1 `shouldBe` now1

            now2 <- C.getCurrentTime
            let contract2 = Contract "SomeContract" "some-network-id" "0x1234" "some-other-abi" now2

            (Entity cId2 c2) <- Sql.runSqlPool (Q.upsertContract contract2) dbPool
            cId2 `shouldBe` toKey (1 :: Int)
            contractAbi c2 `shouldBe` "some-other-abi"
            contractUpdatedAt c2 `shouldBe` now2
