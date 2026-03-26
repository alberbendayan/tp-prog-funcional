module Binance.API.Instance where

import Exchange.Interface
import qualified Binance.API.Client as Client
import Binance.API.Conversion
    ( TickerResult(..)
    , fetchBookTickersForPairs
    , generateAllPairs
    , buildMarketSnapshotWithFees
    , tradeFeeMap
    )
import Bot.Domain (CommissionRate(..))
import Binance.API.Types (AccountInfo(..))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map

data BinanceExchange = BinanceExchange
    { binanceBaseUrl           :: String
    , binanceDefaultCommission :: CommissionRate
    , binanceApiKey            :: String
    , binanceApiSecret         :: String
    }

instance Exchange BinanceExchange where
    checkConnectivity (BinanceExchange url _ _ _) = do
        result <- liftIO $ Client.ping url
        return $ case result of
            Left err -> Left $ ExchangeConnError (show err)
            Right ok -> Right ok

    fetchMarketSnapshot (BinanceExchange url commission apiKey apiSecret) assets = do
        let pairs = generateAllPairs assets
        tickerResults <- liftIO $ fetchBookTickersForPairs url pairs
        let okTickers     = [ bt  | TickerOk bt          <- tickerResults ]
            failedTickers = [ err | TickerFailed err      <- tickerResults ]
        case failedTickers of
            (err:_) -> return $ Left $ ExchangeFetchError (show err)
            []      -> do
                accountResult <- liftIO $ Client.getAccountInfo url apiKey apiSecret
                defaultCommission <- case accountResult of
                    Right info -> do
                        let rate = fromIntegral (accountTakerCommission info) / 10000.0
                        liftIO $ putStrLn $ "Comisión de cuenta: " ++ show rate
                        return $ CommissionRate rate
                    Left err -> do
                        liftIO $ putStrLn $ "Warning: no se pudo obtener comisión de cuenta: " ++ show err ++ ", usando comisión del config"
                        return commission
                feeResult <- liftIO $ Client.getTradeFees url apiKey apiSecret
                feeMap <- case feeResult of
                    Right fees -> return $ tradeFeeMap fees
                    Left _     -> return Map.empty
                return $ Right $ buildMarketSnapshotWithFees okTickers feeMap defaultCommission
