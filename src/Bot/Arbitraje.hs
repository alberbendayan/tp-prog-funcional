{-# LANGUAGE OverloadedStrings #-}

module Bot.Arbitraje
    ( allTriangularPaths
    , simulatePath
    , detectOpportunities
    , makeDecision
    , opportunityToExecutionPlan
    , PlanError(..)
    , validateAndQuantizePlan
    , validatePlanLiquidity
    ) where

import Bot.Domain
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.List (tails, maximumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

allTriangularPaths :: [Asset] -> [TriangularPath]
allTriangularPaths assets =
    concatMap pathsForCombination (combinationsOf3 assets)
  where
    combinationsOf3 :: [a] -> [(a, a, a)]
    combinationsOf3 xs =
        [(x, y, z) | (x:ys) <- tails xs, (y:zs) <- tails ys, z <- zs]

    pathsForCombination :: (Asset, Asset, Asset) -> [TriangularPath]
    pathsForCombination (a, b, c) =
        concatMap pathsForOrder [[a,b,c],[b,c,a],[c,a,b]]

    pathsForOrder :: [Asset] -> [TriangularPath]
    pathsForOrder [x, y, z] = mkAllTriangularPaths x y z
    pathsForOrder _         = []

data SimulatedStep = SimulatedStep
    { simulatedAmountOut   :: AssetQty
    , simulatedBinancePair :: Pair
    , simulatedOrderSide   :: OrderSide
    , simulatedQuantity    :: MarketOrderQty
    }

simulateOneStep
    :: Map.Map Pair PairQuote
    -> Pair
    -> AssetQty
    -> Maybe SimulatedStep
simulateOneStep quotes logicalPair (AssetQty asset qty)

    | asset == base logicalPair
    , Just q <- Map.lookup logicalPair quotes
    , qty <= bidQty q =
        let amountOut = unPrice (bidPrice q) * qty * (1 - unCommissionRate (pairCommission q))
        in Just SimulatedStep
            { simulatedAmountOut   = AssetQty (quote logicalPair) amountOut
            , simulatedBinancePair = logicalPair
            , simulatedOrderSide        = Sell
            , simulatedQuantity         = QtyBase qty
            }

    | asset == quote logicalPair
    , let binancePair = Pair (quote logicalPair) (base logicalPair)
    , Just q <- Map.lookup binancePair quotes
    , qty / unPrice (askPrice q) <= askQty q =
        let amountOut = qty / unPrice (askPrice q) * (1 - unCommissionRate (pairCommission q))
        in Just SimulatedStep
            { simulatedAmountOut   = AssetQty (base logicalPair) amountOut
            , simulatedBinancePair = binancePair
            , simulatedOrderSide        = Buy
            , simulatedQuantity         = QtyQuote qty
            }

    | otherwise = Nothing

simulatePath :: MarketSnapshot -> TriangularPath -> AssetQty -> Maybe AssetQty
simulatePath snapshot path amountIn = do
    s1 <- simulateOneStep quotes (arbPair1 path) amountIn
    s2 <- simulateOneStep quotes (arbPair2 path) (simulatedAmountOut s1)
    s3 <- simulateOneStep quotes (arbPair3 path) (simulatedAmountOut s2)
    return (simulatedAmountOut s3)
  where
    quotes = snapshotQuotes snapshot

stepLookup :: Map.Map Pair PairQuote -> Pair -> Asset -> Maybe (PairQuote, OrderSide, Asset)
stepLookup quotes logicalPair asset
    | asset == base logicalPair
    , Just q <- Map.lookup logicalPair quotes =
        Just (q, Sell, quote logicalPair)
    | asset == quote logicalPair
    , let binancePair = Pair (quote logicalPair) (base logicalPair)
    , Just q <- Map.lookup binancePair quotes =
        Just (q, Buy, base logicalPair)
    | otherwise = Nothing

stepRate :: PairQuote -> OrderSide -> Double
stepRate q Sell = unPrice (bidPrice q) * (1 - unCommissionRate (pairCommission q))
stepRate q Buy  = (1 / unPrice (askPrice q)) * (1 - unCommissionRate (pairCommission q))

stepMaxInput :: PairQuote -> OrderSide -> Double
stepMaxInput q Sell = bidQty q
stepMaxInput q Buy  = askQty q * unPrice (askPrice q)

maxAmountForPath :: Map.Map Pair PairQuote -> TriangularPath -> AssetQty -> Maybe Double
maxAmountForPath quotes path (AssetQty startAsset _) = do
    (q1, side1, asset2) <- stepLookup quotes (arbPair1 path) startAsset
    (q2, side2, asset3) <- stepLookup quotes (arbPair2 path) asset2
    (q3, side3, _)      <- stepLookup quotes (arbPair3 path) asset3
    let rate1        = stepRate q1 side1
        rate2        = stepRate q2 side2
        maxFromStep1 = stepMaxInput q1 side1
        maxFromStep2 = stepMaxInput q2 side2 / rate1
        maxFromStep3 = stepMaxInput q3 side3 / (rate1 * rate2)
    return $ minimum [maxFromStep1, maxFromStep2, maxFromStep3]

detectOpportunities :: [TriangularPath] -> MarketSnapshot -> AssetQty -> [ArbOpportunity]
detectOpportunities paths snapshot amountIn =
    [ ArbOpportunity path cappedIn amountOut
    | path <- paths
    , let cappedIn = capToLiquidity amountIn path
    , Just amountOut <- [simulatePath snapshot path cappedIn]
    , qtyAsset amountOut == qtyAsset amountIn
    , qtyAmount amountOut > qtyAmount cappedIn
    ]
  where
    quotes = snapshotQuotes snapshot
    capToLiquidity (AssetQty a q) path =
        let liqMax = fromMaybe q (maxAmountForPath quotes path (AssetQty a q))
        in AssetQty a (min q liqMax)

makeDecision :: Double -> [ArbOpportunity] -> Decision
makeDecision minProfitPct opps =
    case filter (\o -> arbProfitPerc o >= minProfitPct) opps of
        []   -> NoTrade
        good -> DoTrade $ maximumBy (comparing arbProfitPerc) good

opportunityToExecutionPlan :: MarketSnapshot -> ArbOpportunity -> Maybe ExecutionPlan
opportunityToExecutionPlan snapshot opp = do
    let quotes = snapshotQuotes snapshot
        path   = arbPath opp
    s1 <- simulateOneStep quotes (arbPair1 path) (arbAmountIn opp)
    s2 <- simulateOneStep quotes (arbPair2 path) (simulatedAmountOut s1)
    s3 <- simulateOneStep quotes (arbPair3 path) (simulatedAmountOut s2)
    let step1 = OrderStep (simulatedBinancePair s1) (simulatedOrderSide s1) (simulatedQuantity s1)
        step2 = OrderStep (simulatedBinancePair s2) (simulatedOrderSide s2) (simulatedQuantity s2)
        step3 = OrderStep (simulatedBinancePair s3) (simulatedOrderSide s3) (simulatedQuantity s3)
    return $ mkExecutionPlan path step1 step2 step3

data PlanError
    = StepQtyZeroAfterRound { planErrStep :: Int }
    | StepQtyNegative       { planErrStep :: Int }
    deriving (Show, Eq)

roundToAssetPrecision :: Asset -> Double -> Double
roundToAssetPrecision BTC  q = fromIntegral (floor (q * 1e5) :: Integer) / 1e5
roundToAssetPrecision ETH  q = fromIntegral (floor (q * 1e4) :: Integer) / 1e4
roundToAssetPrecision BNB  q = fromIntegral (floor (q * 1e2) :: Integer) / 1e2
roundToAssetPrecision USDT q = fromIntegral (floor (q * 1e2) :: Integer) / 1e2

quantizeStepQty :: Pair -> MarketOrderQty -> MarketOrderQty
quantizeStepQty pair (QtyBase  q) = QtyBase  (roundToAssetPrecision (base  pair) q)
quantizeStepQty pair (QtyQuote q) = QtyQuote (roundToAssetPrecision (quote pair) q)

validateStep :: Int -> OrderStep -> Either PlanError OrderStep
validateStep stepNum step =
    let quantized    = quantizeStepQty (stepPair step) (stepQty step)
        quantizedAmt = case quantized of { QtyBase q -> q; QtyQuote q -> q }
        roundedStep  = step { stepQty = quantized }
    in if quantizedAmt <= 0
       then Left $ StepQtyZeroAfterRound stepNum
       else Right roundedStep

validateAndQuantizePlan :: ExecutionPlan -> Either PlanError ExecutionPlan
validateAndQuantizePlan plan = do
    s1 <- validateStep 1 (planStep1 plan)
    s2 <- validateStep 2 (planStep2 plan)
    s3 <- validateStep 3 (planStep3 plan)
    return $ mkExecutionPlan (planPath plan) s1 s2 s3

validatePlanLiquidity :: MarketSnapshot -> ExecutionPlan -> Either T.Text ()
validatePlanLiquidity snapshot plan =
    mapM_ checkStep (zip [1 :: Int ..] (executionPlanSteps plan))
  where
    quotes = snapshotQuotes snapshot

    checkStep :: (Int, OrderStep) -> Either T.Text ()
    checkStep (n, step) =
        case Map.lookup (stepPair step) quotes of
            Nothing -> Left $ "paso " <> T.pack (show n) <> ": par no disponible en snapshot"
            Just q  -> checkLiquidity n q (stepSide step) (stepQty step)

    checkLiquidity :: Int -> PairQuote -> OrderSide -> MarketOrderQty -> Either T.Text ()
    checkLiquidity n q Sell (QtyBase qty)
        | qty <= bidQty q = Right ()
        | otherwise       = Left $ "paso " <> T.pack (show n) <> ": liquidez insuficiente (sell "
                                <> T.pack (show qty) <> " > bidQty " <> T.pack (show (bidQty q)) <> ")"
    checkLiquidity n q Buy (QtyQuote qty)
        | qty / unPrice (askPrice q) <= askQty q = Right ()
        | otherwise = Left $ "paso " <> T.pack (show n) <> ": liquidez insuficiente (buy, baseNeeded "
                          <> T.pack (show (qty / unPrice (askPrice q)))
                          <> " > askQty " <> T.pack (show (askQty q)) <> ")"
    checkLiquidity n _ _ _ =
        Left $ "paso " <> T.pack (show n) <> ": combinacion inesperada de side/qty"
