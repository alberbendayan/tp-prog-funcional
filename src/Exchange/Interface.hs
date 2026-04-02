module Exchange.Interface where

import Bot.Domain (MarketSnapshot, OrderStep, Fill)
import Binance.API.Types (Asset)
import Control.Monad.IO.Class (MonadIO)

data ExchangeError
    = ExchangeConnError String
    | ExchangeFetchError String
    | ExchangeOrderError String
    deriving (Show, Eq)

class Exchange e where
    checkConnectivity
        :: MonadIO m
        => e -> m (Either ExchangeError Bool)

    fetchMarketSnapshot
        :: MonadIO m
        => e -> [Asset] -> m (Either ExchangeError MarketSnapshot)

    executeOrder
        :: MonadIO m
        => e -> OrderStep -> m (Either ExchangeError Fill)
