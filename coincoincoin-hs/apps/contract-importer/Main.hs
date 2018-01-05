module Main where

import qualified Control.Concurrent.Async as A
import qualified Control.Monad as M
import qualified Data.Aeson as AE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Either as E
import qualified System.Directory as D
import qualified System.FilePath as FP

import Truffle.Types (BuildArtifact)

import AppConfig (AppConfig(..), loadConfig)

main :: IO ()
main = do
    config <- loadConfig
    buildArtifacts <- fmap AE.eitherDecodeStrict <$> loadBuildArtifacts (contractsPath config) :: IO [Either String BuildArtifact]
    print config
    print . length $ E.lefts buildArtifacts
    print . length $ E.rights buildArtifacts

loadBuildArtifacts :: FilePath -> IO [ByteString]
loadBuildArtifacts path = do
    isDir <- D.doesDirectoryExist path
    if isDir
    then do
        files <- fmap (path FP.</>) <$> D.listDirectory path
        M.join <$> A.mapConcurrently loadBuildArtifacts files
    else
        (:[]) <$> BS.readFile path
