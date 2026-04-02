module Bot.Arbitraje
    ( allTriangularPaths
    , simulatePath
    , detectOpportunities
    , makeDecision
    , opportunityToExecutionPlan
    , PlanError(..)
    , validateAndQuantizePlan
    ) where

import Bot.Domain
import qualified Data.Map.Strict as Map
import Data.List (tails, maximumBy, permutations)
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
        concatMap pathsForOrder (permutations [a, b, c])

    pathsForOrder :: [Asset] -> [TriangularPath]
    pathsForOrder [x, y, z] = mkAllTriangularPaths x y z
    pathsForOrder _         = []

data SimulatedStep = SimulatedStep
    { simulatedAmountOut   :: AssetQty        -- cuánto recibimos tras el paso
    , simulatedBinancePair :: Pair            -- el par que existe en Binance (puede ser el inverso del lógico)
    , simulatedOrderSide   :: OrderSide       -- Buy o Sell según la orientación del par
    , simulatedQuantity    :: MarketOrderQty  -- parámetro a enviar a Binance
    }

simulateOneStep
    :: Map.Map Pair PairQuote
    -> Pair  
    -> AssetQty
    -> Maybe SimulatedStep
simulateOneStep quotes logicalPair (AssetQty asset qty)

    | asset == base logicalPair
    , Just q <- Map.lookup logicalPair quotes =
        let amountOut = unPrice (bidPrice q) * qty * (1 - unCommissionRate (pairCommission q))
        in Just SimulatedStep
            { simulatedAmountOut   = AssetQty (quote logicalPair) amountOut
            , simulatedBinancePair = logicalPair
            , simulatedOrderSide        = Sell
            , simulatedQuantity         = QtyBase qty
            }

    | asset == quote logicalPair
    , let binancePair = Pair (quote logicalPair) (base logicalPair)
    , Just q <- Map.lookup binancePair quotes =
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

detectOpportunities :: [TriangularPath] -> MarketSnapshot -> AssetQty -> [ArbOpportunity]
detectOpportunities paths snapshot amountIn =
    [ ArbOpportunity path amountIn amountOut
    | path <- paths
    , Just amountOut <- [simulatePath snapshot path amountIn]
    , qtyAsset amountOut == qtyAsset amountIn
    , qtyAmount amountOut > qtyAmount amountIn
    ]

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
