module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import AppConfig (AppConfig(..), loadConfig)


main :: IO ()
main = do
    config <- loadConfig
    buildArtifacts <- loadBuildArtifacts $ contractsPath config
    print config
    print $ length buildArtifacts

loadBuildArtifacts :: FilePath -> IO [ByteString]
loadBuildArtifacts path =
    (:[]) <$> BS.readFile path
