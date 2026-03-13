module Bot.Arbitraje
    ( allTriangularPaths
    , simulatePath
    , detectOpportunities
    , makeDecision
    ) where

import Bot.Domain
import Binance.API.Types (Asset, Price(..))
import qualified Data.Map.Strict as Map
import Data.List (tails, maximumBy)
import Data.Ord (comparing)

allTriangularPaths :: [Asset] -> [TriangularPath]
allTriangularPaths assets =
    concatMap (\(a, b, c) -> mkAllTriangularPaths a b c) (triples assets)

triples :: [a] -> [(a, a, a)]
triples xs = [(x, y, z) | (x:ys) <- tails xs, (y:zs) <- tails ys, z <- zs]

simulatePath :: MarketSnapshot -> TriangularPath -> Double -> Maybe Double
simulatePath snapshot path amountIn = do
    q1 <- Map.lookup (arbPair1 path) (snapshotQuotes snapshot)
    q2 <- Map.lookup (arbPair2 path) (snapshotQuotes snapshot)
    q3 <- Map.lookup (arbPair3 path) (snapshotQuotes snapshot)
    let step qty q = unPrice (bidPrice q) * qty * (1 - unCommissionRate (pairCommission q))
    return $ step (step (step amountIn q1) q2) q3

detectOpportunities :: [TriangularPath] -> MarketSnapshot -> Double -> [ArbOpportunity]
detectOpportunities paths snapshot amountIn =
    [ ArbOpportunity path amountIn amountOut
    | path <- paths
    , Just amountOut <- [simulatePath snapshot path amountIn]
    , amountOut > amountIn
    ]

makeDecision :: Double -> [ArbOpportunity] -> Decision
makeDecision minProfitPct opps =
    case filter (\o -> arbProfitPerc o >= minProfitPct) opps of
        []   -> NoTrade
        good -> DoTrade $ maximumBy (comparing arbProfitPerc) good
