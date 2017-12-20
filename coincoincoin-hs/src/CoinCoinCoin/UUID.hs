{-# OPTIONS_GHC -fno-warn-orphans #-}

module CoinCoinCoin.UUID
    ( UUID
    , fromString
    , module Data.UUID.V4
    ) where

import Data.UUID (UUID, fromString)
import Data.UUID.V4
