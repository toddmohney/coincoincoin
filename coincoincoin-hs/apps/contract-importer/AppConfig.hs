module AppConfig
    ( AppConfig(..)
    , loadConfig
    ) where

import qualified Data.ByteString.Char8 as C8
import Options.Applicative
import Data.Monoid ((<>))

import CoinCoinCoin.Database.Models (ConnectionString)

data AppConfig = AppConfig
    { dbConnectionString :: ConnectionString
    , contractsPath :: FilePath
    } deriving (Show)

loadConfig :: IO AppConfig
loadConfig = execParser programOpts

programOpts :: ParserInfo AppConfig
programOpts =
    info
        (optsParser <**> helper)
        ( fullDesc
            <> progDesc "Import solidity build artifacts"
            <> header "contract-importer - A Solidity Build Artifact Importer"
        )

optsParser :: Parser AppConfig
optsParser =
    AppConfig
        <$> (C8.pack <$> strOption connectionStringParser)
        <*> argument str filepathParser

    where
        connectionStringParser =
            long "connection-string"
                <> short 'c'
                <> help "Connection to Postgresql database"

        filepathParser =
            metavar "SOURCE"
