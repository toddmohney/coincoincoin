module CoinCoinCoin.MessageQueue.Types
    ( Topic(..)
    ) where

data Topic = GitHubWebhookReceived
           | LinkGitHubCommandReceived
    deriving (Show, Eq, Read)
