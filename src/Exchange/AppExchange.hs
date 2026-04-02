{-# LANGUAGE ExistentialQuantification #-}

module Exchange.AppExchange
  ( AppExchange(..)
  , configureAppExchange
  ) where

import Bot.Config (Config(..), ExchangeKind(..))
import Bot.Domain (CommissionRate(..))
import Exchange.Interface
import Binance.API.Instance (BinanceExchange(..))
import FakeExchange.Control (newDemoFakeExchange)

-- | Cualquier implementación de 'Exchange' (Binance, Fake, OKX, …) en un solo tipo.
-- Nuevo exchange: instancia 'Exchange', suma un caso en 'configureAppExchange' y listo;
-- esta instancia no crece con cada proveedor.
data AppExchange = forall e. Exchange e => AppExchange e

instance Exchange AppExchange where
    checkConnectivity (AppExchange e) = checkConnectivity e
    fetchMarketSnapshot (AppExchange e) = fetchMarketSnapshot e
    executeOrder (AppExchange e) = executeOrder e

configureAppExchange :: Config -> IO AppExchange
configureAppExchange cfg = case cfgExchangeKind cfg of
    ExchangeKindBinance ->
        return $ AppExchange BinanceExchange
            { binanceBaseUrl           = cfgBaseUrl cfg
            , binanceDefaultCommission = CommissionRate (cfgCommissionRate cfg)
            , binanceApiKey            = cfgApiKey cfg
            , binanceApiSecret         = cfgApiSecret cfg
            }
    ExchangeKindFake ->
        AppExchange <$> newDemoFakeExchange
