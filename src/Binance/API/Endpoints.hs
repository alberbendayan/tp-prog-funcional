{-# LANGUAGE OverloadedStrings #-}

module Binance.API.Endpoints
    ( pingEndpoint
    , tickerPriceEndpoint
    , bookTickerEndpoint
    ) where

import Data.Text (Text)

pingEndpoint :: Text
pingEndpoint = "/api/v3/ping"

tickerPriceEndpoint :: Text
tickerPriceEndpoint = "/api/v3/ticker/price"

bookTickerEndpoint :: Text
bookTickerEndpoint = "/api/v3/ticker/bookTicker"
