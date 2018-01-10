module AppConfig
    ( AppConfig(..)
    , loadConfig
    ) where

import qualified Data.ByteString.Char8 as C8
import           Data.Monoid ((<>))
import           Options.Applicative

import CoinCoinCoin.Database.Config (ConnectionPool, mkPool')
import CoinCoinCoin.Database.Models (ConnectionString)

data AppConfig = AppConfig
    { appDbConn        :: ConnectionPool
    , appContractsPath :: FilePath
    } deriving (Show)

data CLIOpts = CLIOpts
    { dbConnectionString :: ConnectionString
    , contractsPath      :: FilePath
    } deriving (Show)

loadConfig :: IO AppConfig
loadConfig = execParser programOpts >>= mkAppConfig

mkAppConfig :: CLIOpts -> IO AppConfig
mkAppConfig CLIOpts{..} = do
    dbPool <- mkPool' dbConnectionString
    return $ AppConfig dbPool contractsPath

programOpts :: ParserInfo CLIOpts
programOpts =
    info
        (optsParser <**> helper)
        ( fullDesc
            <> progDesc "Import solidity build artifacts"
            <> header "contract-importer - A Solidity Build Artifact Importer"
        )

optsParser :: Parser CLIOpts
optsParser =
    CLIOpts
        <$> (C8.pack <$> strOption connectionStringParser)
        <*> argument str filepathParser

    where
        connectionStringParser =
            long "connection-string"
                <> short 'c'
                <> help "Connection to Postgresql database"

        filepathParser =
            metavar "SOURCE"
