{-# LANGUAGE OverloadedStrings #-}

module Binance.API.Endpoints
    ( pingEndpoint
    , tickerPriceEndpoint
    , accountEndpoint
    ) where

import Data.Text (Text)

pingEndpoint :: Text
pingEndpoint = "/api/v3/ping"

tickerPriceEndpoint :: Text
tickerPriceEndpoint = "/api/v3/ticker/price"

accountEndpoint :: Text
accountEndpoint = "/api/v3/account"
