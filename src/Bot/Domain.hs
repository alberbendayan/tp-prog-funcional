{-# LANGUAGE DeriveGeneric #-}

module Bot.Domain
  ( MarketSnapshot(..)
  , PairQuote(..)
  , CommissionRate(..)
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
  ) where

import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Binance.API.Types (Pair(..), Asset(..), Price(..))

newtype CommissionRate = CommissionRate { unCommissionRate :: Double }
  deriving (Show, Eq, Ord, Generic)

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
  , arbAmountIn  :: Double
  , arbAmountOut :: Double
  } deriving (Show, Eq, Generic)

arbProfitAbs :: ArbOpportunity -> Double
arbProfitAbs o = arbAmountOut o - arbAmountIn o

arbProfitPerc :: ArbOpportunity -> Double
arbProfitPerc o
  | arbAmountIn o > 0 = 100 * arbProfitAbs o / arbAmountIn o
  | otherwise         = 0

data Decision
  = NoTrade
  | DoTrade ArbOpportunity
  deriving (Show, Eq, Generic)

data OrderSide = Buy | Sell --sobre el base asset
  deriving (Show, Eq, Ord, Generic)

data OrderStep = OrderStep
  { stepPair       :: Pair
  , stepSide       :: OrderSide
  , stepAmountBase :: Double
  } deriving (Show, Eq, Generic)

data ExecutionPlan = ExecutionPlan
  { planPath    :: TriangularPath
  , planAmount1 :: Double
  , planAmount2 :: Double
  , planAmount3 :: Double
  } deriving (Show, Eq, Generic)

mkExecutionPlan :: TriangularPath -> Double -> Double -> Double -> ExecutionPlan
mkExecutionPlan path a1 a2 a3 = ExecutionPlan path a1 a2 a3

executionPlanSteps :: ExecutionPlan -> [OrderStep]
executionPlanSteps ep =
  [ OrderStep (arbPair1 $ planPath ep) Sell (planAmount1 ep)
  , OrderStep (arbPair2 $ planPath ep) Sell (planAmount2 ep)
  , OrderStep (arbPair3 $ planPath ep) Sell (planAmount3 ep)
  ]

data Fill = Fill
  { fillPair       :: Pair
  , fillSide       :: OrderSide
  , fillAmountBase :: Double
  , fillPrice      :: Price
  , fillFee        :: Double
  , fillTime       :: UTCTime
  } deriving (Show, Eq, Generic)

data RoundStatus
  = RoundSuccess
  | RoundPartial [String]
  | RoundFailed String
  deriving (Show, Eq, Generic)

data RoundResult = RoundResult
  { roundFills     :: [Fill]
  , roundAmountIn  :: Double
  , roundAmountOut :: Double
  , roundStatus    :: RoundStatus
  } deriving (Show, Eq, Generic)

roundPnl :: RoundResult -> Double
roundPnl r = roundAmountOut r - roundAmountIn r
