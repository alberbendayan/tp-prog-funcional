module Binance.API.Instance where

import Exchange.Interface
import qualified Binance.API.Client as Client
import Binance.API.Conversion (TickerResult(..), fetchBookTickersForPairs, generateAllPairs, buildMarketSnapshot)
import Bot.Domain (CommissionRate)
import Control.Monad.IO.Class (MonadIO, liftIO)

data BinanceExchange = BinanceExchange
    { binanceBaseUrl           :: String
    , binanceDefaultCommission :: CommissionRate
    }

instance Exchange BinanceExchange where
    checkConnectivity (BinanceExchange url _) = do
        result <- liftIO $ Client.ping url
        return $ case result of
            Left err -> Left $ ExchangeConnError (show err)
            Right ok -> Right ok

    fetchMarketSnapshot (BinanceExchange url commission) assets = do
        let pairs = generateAllPairs assets
        tickerResults <- liftIO $ fetchBookTickersForPairs url pairs
        let okTickers      = [ bt  | TickerOk bt        <- tickerResults ]
            failedTickers  = [ err | TickerFailed err   <- tickerResults ]
            _unsupported   = [ s   | TickerNotSupported s <- tickerResults ]
        case failedTickers of
          (err:_) -> return $ Left $ ExchangeFetchError (show err)
          []      -> return $ Right $ buildMarketSnapshot okTickers commission
