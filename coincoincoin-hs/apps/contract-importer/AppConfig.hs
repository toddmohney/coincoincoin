module AppConfig
    ( AppConfig(..)
    , loadConfig
    ) where

import Options.Applicative
import Data.Monoid ((<>))

import CoinCoinCoin.Database.Config as DB
import CoinCoinCoin.Database.Models (ConnectionString)

data AppConfig = AppConfig
    { connectionString :: ConnectionString
    , contractsPath :: FilePath
    } deriving (Show)

{- Load configuation information from either the environment
 - or supplied command line arguments.
 - Command line arguments take precedence over environment configuration.
 -}
loadConfig :: IO AppConfig
loadConfig = do
    dbConn <- DB.dbConnectionString'
    execParser $ programOpts dbConn

programOpts :: Maybe ConnectionString -> ParserInfo AppConfig
programOpts dbConn =
    info
        (optsParser dbConn <**> helper)
        ( fullDesc
            <> progDesc "Import solidity build artifacts"
            <> header "contract-importer - A Solidity Build Artifact Importer"
        )

optsParser :: Maybe ConnectionString -> Parser AppConfig
optsParser mConn = case mConn of
    Nothing ->
        AppConfig
            <$> option auto connectionStringParser
            <*> argument auto filepathParser
    (Just conn) ->
        AppConfig
            <$> option auto
                ( connectionStringParser
                    <> showDefault
                    <> value conn
                )
            <*> argument auto filepathParser

    where
        connectionStringParser =
            long "connection-string"
                <> short 'c'
                <> help "Connection to Postgresql database"

        filepathParser =
            metavar "SOURCE"
