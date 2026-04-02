{-# LANGUAGE OverloadedStrings #-}

module Binance.API.Endpoints
    ( pingEndpoint
    , bookTickerEndpoint
    , tradeFeeEndpoint
    , accountEndpoint
    , orderEndpoint
    ) where

import Data.Text (Text)

pingEndpoint :: Text
pingEndpoint = "/api/v3/ping"

bookTickerEndpoint :: Text
bookTickerEndpoint = "/api/v3/ticker/bookTicker"

tradeFeeEndpoint :: Text
tradeFeeEndpoint = "/sapi/v1/asset/tradeFee"

accountEndpoint :: Text
accountEndpoint = "/api/v3/account"

orderEndpoint :: Text
orderEndpoint = "/api/v3/order"
