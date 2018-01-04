module Main where

import AppConfig (loadConfig)


main :: IO ()
main = do
    config <- loadConfig
    print config
