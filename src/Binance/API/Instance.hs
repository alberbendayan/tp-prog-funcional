module Binance.API.Instance where

import Exchange.Interface
import qualified Binance.API.Client as Client
import Binance.API.Conversion (fetchBookTickersForPairs, generateAllPairs, buildMarketSnapshot)
import Bot.Domain (CommissionRate)

data BinanceExchange = BinanceExchange
    { binanceBaseUrl           :: String
    , binanceDefaultCommission :: CommissionRate
    }

instance Exchange BinanceExchange where
    checkConnectivity (BinanceExchange url _) = do
        result <- Client.ping url
        return $ case result of
            Left err -> Left $ ExchangeConnError (show err)
            Right ok -> Right ok

    fetchMarketSnapshot (BinanceExchange url commission) assets = do
        let pairs = generateAllPairs assets
        tickers <- fetchBookTickersForPairs url pairs
        return $ Right $ buildMarketSnapshot tickers commission
