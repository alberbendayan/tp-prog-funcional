module Exchange.Interface where

import Bot.Domain (Asset, MarketSnapshot, OrderStep, Fill)
import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (Map)
import Data.Text (Text)

data ExchangeError
    = ExchangeConnError Text
    | ExchangeFetchError Text
    | ExchangeOrderError Text
    deriving (Show, Eq)

class Exchange e where
    checkConnectivity
        :: MonadIO m
        => e -> m (Either ExchangeError Bool)

    fetchMarketSnapshot
        :: MonadIO m
        => e -> [Asset] -> m (Either ExchangeError MarketSnapshot)

    fetchBalances
        :: MonadIO m
        => e -> m (Either ExchangeError (Map Asset Double))

    executeOrder
        :: MonadIO m
        => e -> OrderStep -> m (Either ExchangeError Fill)
