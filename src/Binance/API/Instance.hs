{-# LANGUAGE OverloadedStrings #-}

module Binance.API.Instance where

import Exchange.Interface
import qualified Binance.API.Client as Client
import Binance.API.Conversion
    ( TickerResult(..)
    , fetchBookTickersForPairs
    , generateAllPairs
    , buildMarketSnapshotWithFees
    , tradeFeeMap
    , orderResponseToFill
    )
import Bot.Domain (CommissionRate(..), OrderStep(..), OrderSide(..), Fill(..))
import Data.Bifunctor (first)
import Binance.API.Types (AccountInfo(..), OrderResponse(..), pairToSymbol, Symbol(..))
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Time.Clock (getCurrentTime, UTCTime)
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

    executeOrder (BinanceExchange url _ apiKey apiSecret) step = do
        let symbol = T.unpack $ unSymbol $ pairToSymbol (stepPair step)
        orderResult <- liftIO $ Client.placeMarketOrder url apiKey apiSecret (T.pack symbol) (sideToText $ stepSide step) (stepQty step)
        processOrderResult step orderResult

processOrderResult :: MonadIO m => OrderStep -> Either Client.BinanceError OrderResponse -> m (Either ExchangeError Fill)
processOrderResult _    (Left err)   = return $ Left $ ExchangeOrderError (show err)
processOrderResult step (Right resp) = fillFromResponse resp step <$> liftIO getCurrentTime

sideToText :: OrderSide -> T.Text
sideToText Sell = "SELL"
sideToText Buy  = "BUY"

fillFromResponse :: OrderResponse -> OrderStep -> UTCTime -> Either ExchangeError Fill
fillFromResponse resp step now =
    first ExchangeOrderError $ orderResponseToFill resp (stepSide step) (stepPair step) now
