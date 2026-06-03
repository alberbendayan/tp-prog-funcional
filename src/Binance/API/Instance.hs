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
import Bot.Domain (CommissionRate(..), MarketSnapshot, OrderStep(..), OrderSide(..), Fill(..))
import Data.Bifunctor (bimap, first)
import Binance.API.Types (AccountInfo(..), BookTicker, OrderResponse(..), pairToSymbol, Symbol(..), accountBalances)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Time.Clock (getCurrentTime, UTCTime)
import qualified Data.Map.Strict as Map

data BinanceExchange = BinanceExchange
    { binanceBaseUrl           :: T.Text
    , binanceDefaultCommission :: CommissionRate
    , binanceApiKey            :: T.Text
    , binanceApiSecret         :: T.Text
    }

instance Exchange BinanceExchange where
    checkConnectivity (BinanceExchange url _ _ _) =
        liftIO $ first (ExchangeConnError . T.pack . show) <$> Client.ping url

    fetchMarketSnapshot (BinanceExchange url commission apiKey apiSecret) assets = do
        tickers <- liftIO $ fetchBookTickersForPairs url (generateAllPairs assets)
        case [err | TickerFailed err <- tickers] of
            (err:_) -> return $ Left $ ExchangeFetchError (T.pack $ show err)
            []      -> Right <$> buildSnapshot url commission apiKey apiSecret [bt | TickerOk bt <- tickers]

    fetchBalances (BinanceExchange url _ apiKey apiSecret) =
        liftIO $ bimap (ExchangeFetchError . T.pack . show) accountBalances
               <$> Client.getAccountInfo url apiKey apiSecret

    executeOrder (BinanceExchange url _ apiKey apiSecret) step =
        liftIO (Client.placeMarketOrder url apiKey apiSecret sym side qty) >>= processOrderResult step
      where
        sym  = unSymbol $ pairToSymbol (stepPair step)
        side = sideToText (stepSide step)
        qty  = stepQty step

buildSnapshot :: MonadIO m => T.Text -> CommissionRate -> T.Text -> T.Text -> [BookTicker] -> m MarketSnapshot
buildSnapshot url commission apiKey apiSecret okTickers = do
    defaultCommission <- liftIO (Client.getAccountInfo url apiKey apiSecret) >>= resolveCommission commission
    feeMap <- either (const Map.empty) tradeFeeMap <$> liftIO (Client.getTradeFees url apiKey apiSecret)
    return $ buildMarketSnapshotWithFees okTickers feeMap defaultCommission

resolveCommission :: MonadIO m => CommissionRate -> Either Client.BinanceError AccountInfo -> m CommissionRate
resolveCommission _ (Right info) = do
    let rate = fromIntegral (accountTakerCommission info) / 10000.0
    liftIO $ putStrLn $ "Comisión de cuenta: " ++ show rate
    return $ CommissionRate rate
resolveCommission fallback (Left err) = do
    liftIO $ putStrLn $ "Warning: no se pudo obtener comisión de cuenta: " ++ show err ++ ", usando comisión del config"
    return fallback

processOrderResult :: MonadIO m => OrderStep -> Either Client.BinanceError OrderResponse -> m (Either ExchangeError Fill)
processOrderResult _    (Left err)   = return $ Left $ ExchangeOrderError (T.pack $ show err)
processOrderResult step (Right resp) = fillFromResponse resp step <$> liftIO getCurrentTime

sideToText :: OrderSide -> T.Text
sideToText Sell = "SELL"
sideToText Buy  = "BUY"

fillFromResponse :: OrderResponse -> OrderStep -> UTCTime -> Either ExchangeError Fill
fillFromResponse resp step now =
    first ExchangeOrderError $ orderResponseToFill resp (stepSide step) (stepPair step) now
