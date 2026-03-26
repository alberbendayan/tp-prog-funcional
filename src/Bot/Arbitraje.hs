module Bot.Arbitraje
    ( allTriangularPaths
    , simulatePath
    , detectOpportunities
    , makeDecision
    ) where

import Bot.Domain
import Binance.API.Types (Asset, Pair(..), Price(..))
import qualified Data.Map.Strict as Map
import Data.List (tails, maximumBy)
import Data.Ord (comparing)

allTriangularPaths :: [Asset] -> [TriangularPath]
allTriangularPaths assets =
    concatMap (\(a, b, c) -> mkAllTriangularPaths a b c) (triples assets)

triples :: [a] -> [(a, a, a)]
triples xs = [(x, y, z) | (x:ys) <- tails xs, (y:zs) <- tails ys, z <- zs]

simulateSellBaseToQuote
    :: Pair
    -> PairQuote
    -> AssetQty
    -> Maybe AssetQty
simulateSellBaseToQuote pair q (AssetQty a qty)
    | a == base pair =
        Just $ AssetQty
            { qtyAsset  = quote pair
            , qtyAmount = unPrice (bidPrice q) * qty * (1 - unCommissionRate (pairCommission q))
            }
    | otherwise = Nothing

simulatePath :: MarketSnapshot -> TriangularPath -> AssetQty -> Maybe AssetQty
simulatePath snapshot path amountIn = do
    q1 <- Map.lookup (arbPair1 path) (snapshotQuotes snapshot)
    q2 <- Map.lookup (arbPair2 path) (snapshotQuotes snapshot)
    q3 <- Map.lookup (arbPair3 path) (snapshotQuotes snapshot)
    let p1 = arbPair1 path
        p2 = arbPair2 path
        p3 = arbPair3 path
    a1 <- simulateSellBaseToQuote p1 q1 amountIn
    a2 <- simulateSellBaseToQuote p2 q2 a1
    a3 <- simulateSellBaseToQuote p3 q3 a2
    return a3

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
