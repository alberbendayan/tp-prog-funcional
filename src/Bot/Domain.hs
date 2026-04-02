{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Domain
  ( -- * Mercado (compartido entre exchanges)
    Asset(..)
  , Pair(..)
  , Price(..)
  , MarketOrderQty(..)
  , MarketSnapshot(..)
  , PairQuote(..)
  , CommissionRate(..)
  , AssetQty(..)
  , TriangularPath
  , arbPair1
  , arbPair2
  , arbPair3
  , mkTriangularPath
  , mkAllTriangularPaths
  , ArbOpportunity(..)
  , arbProfitAbs
  , arbProfitPerc
  , Decision(..)
  , OrderSide(..)
  , OrderStep(..)
  , ExecutionPlan(..)
  , mkExecutionPlan
  , executionPlanSteps
  , Fill(..)
  , RoundStatus(..)
  , RoundResult(..)
  , roundPnl
  , roundPnlAmount
  ) where

import Data.Aeson (FromJSON(..), Value(..))
import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

-- | Activo negociable (mismo dominio para cualquier exchange).
data Asset = BTC | ETH | USDT | BNB
  deriving (Show, Eq, Ord)

instance FromJSON Asset where
  parseJSON (String "BTC")  = return BTC
  parseJSON (String "ETH")  = return ETH
  parseJSON (String "USDT") = return USDT
  parseJSON (String "BNB")  = return BNB
  parseJSON (String s)      = fail $ "Asset desconocido: " ++ T.unpack s
  parseJSON _               = fail "Asset debe ser un string"

-- | Par base/cotización (ej. BTC respecto de USDT).
data Pair = Pair { base :: Asset, quote :: Asset }
  deriving (Show, Eq, Ord)

newtype Price = Price { unPrice :: Double }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Price where
  parseJSON (String s) = case reads (T.unpack s) of
    [(d, "")] -> return $ Price d
    _         -> fail "Invalid price string"
  parseJSON v = Price <$> parseJSON v

data MarketOrderQty
    = QtyBase  Double
    | QtyQuote Double
    deriving (Show, Eq)

newtype CommissionRate = CommissionRate { unCommissionRate :: Double }
  deriving (Show, Eq, Ord, Generic)

-- | Quantity tagged with the asset it refers to.
-- | This makes units explicit when reading function signatures.
data AssetQty = AssetQty
  { qtyAsset  :: Asset
  , qtyAmount :: Double
  } deriving (Show, Eq, Ord, Generic)

data PairQuote = PairQuote
  { bidPrice         :: Price
  , askPrice         :: Price
  , pairCommission   :: CommissionRate
  } deriving (Show, Eq, Ord, Generic)

data MarketSnapshot = MarketSnapshot
  { snapshotQuotes :: Map Pair PairQuote
  } deriving (Show, Eq, Generic)

data TriangularPath = TriangularPath
  { arbPair1 :: Pair
  , arbPair2 :: Pair
  , arbPair3 :: Pair
  } deriving (Show, Eq, Ord, Generic)

isValidCycle :: Pair -> Pair -> Pair -> Bool
isValidCycle p1 p2 p3 =
  quote p1 == base p2 && quote p2 == base p3 && quote p3 == base p1

mkTriangularPath :: Pair -> Pair -> Pair -> Maybe TriangularPath
mkTriangularPath p1 p2 p3
  | isValidCycle p1 p2 p3 = Just $ TriangularPath p1 p2 p3
  | otherwise             = Nothing

mkAllTriangularPaths :: Asset -> Asset -> Asset -> [TriangularPath]
mkAllTriangularPaths a b c =
  [ fromJust $ mkTriangularPath (Pair a b) (Pair b c) (Pair c a)
  , fromJust $ mkTriangularPath (Pair a c) (Pair c b) (Pair b a)
  ]

data ArbOpportunity = ArbOpportunity
  { arbPath      :: TriangularPath
  , arbAmountIn  :: AssetQty
  , arbAmountOut :: AssetQty
  } deriving (Show, Eq, Generic)

arbProfitAbs :: ArbOpportunity -> Double
arbProfitAbs o = qtyAmount (arbAmountOut o) - qtyAmount (arbAmountIn o)

arbProfitPerc :: ArbOpportunity -> Double
arbProfitPerc o
  | qtyAmount (arbAmountIn o) > 0 = 100 * arbProfitAbs o / qtyAmount (arbAmountIn o)
  | otherwise         = 0

data Decision
  = NoTrade
  | DoTrade ArbOpportunity
  deriving (Show, Eq, Generic)

data OrderSide = Buy | Sell
  deriving (Show, Eq, Ord, Generic)

data OrderStep = OrderStep
  { stepPair :: Pair
  , stepSide :: OrderSide
  , stepQty  :: MarketOrderQty
  } deriving (Show, Eq, Generic)

data ExecutionPlan = ExecutionPlan
  { planPath  :: TriangularPath
  , planStep1 :: OrderStep
  , planStep2 :: OrderStep
  , planStep3 :: OrderStep
  } deriving (Show, Eq, Generic)

mkExecutionPlan :: TriangularPath -> OrderStep -> OrderStep -> OrderStep -> ExecutionPlan
mkExecutionPlan = ExecutionPlan

executionPlanSteps :: ExecutionPlan -> [OrderStep]
executionPlanSteps ep = [planStep1 ep, planStep2 ep, planStep3 ep]

data Fill = Fill
  { fillPair       :: Pair
  , fillSide       :: OrderSide
  , fillAmountBase :: Double
  , fillPrice      :: Price
  , fillFee        :: Double
  , fillFeeAsset   :: Asset
  , fillTime       :: UTCTime
  } deriving (Show, Eq, Generic)

data RoundStatus
  = RoundSuccess
  | RoundPartial [String]
  | RoundFailed String
  deriving (Show, Eq, Generic)

data RoundResult = RoundResult
  { roundFills     :: [Fill]
  , roundAmountIn  :: AssetQty
  , roundAmountOut :: AssetQty
  , roundStatus    :: RoundStatus
  } deriving (Show, Eq, Generic)

roundPnl :: RoundResult -> Either String AssetQty
roundPnl r
  | qtyAsset (roundAmountIn r) /= qtyAsset (roundAmountOut r) =
      Left "roundPnl: roundAmountIn/out tienen assets distintos"
  | otherwise =
      Right $ AssetQty
        { qtyAsset  = qtyAsset (roundAmountIn r)
        , qtyAmount = qtyAmount (roundAmountOut r) - qtyAmount (roundAmountIn r)
        }

roundPnlAmount :: RoundResult -> Either String Double
roundPnlAmount r = qtyAmount <$> roundPnl r
