{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Test.QuickCheck
import qualified Data.Map.Strict as Map
import Data.List (nub)

import Bot.Domain
import Bot.Arbitraje
import Bot.Pricing


instance Arbitrary Asset where
    arbitrary = elements [BTC, ETH, BNB, USDT]

instance Arbitrary Price where
    arbitrary = Price . abs <$> (arbitrary `suchThat` (> 0))

instance Arbitrary CommissionRate where
    arbitrary = CommissionRate <$> choose (0, 0.01)

instance Arbitrary PairQuote where
    arbitrary = do
        bid <- choose (0.0001, 100000)
        spread <- choose (0, bid * 0.01)
        let ask = bid + spread
        fee <- choose (0, 0.01)
        bQty <- choose (0.001, 10000)
        aQty <- choose (0.001, 10000)
        return $ PairQuote (Price bid) (Price ask) (CommissionRate fee) bQty aQty


instance Arbitrary ProfitPct where
    arbitrary = ProfitPct . abs <$> arbitrary

-- makeDecision con lista vacía siempre devuelve NoTrade,
-- sin importar el umbral.
prop_noTradeOnEmpty :: ProfitPct -> Bool
prop_noTradeOnEmpty threshold = makeDecision threshold [] == NoTrade


-- Todos los caminos generados por allTriangularPaths forman ciclos válidos:
-- quote(par1) == base(par2), quote(par2) == base(par3), quote(par3) == base(par1).
prop_pathsAreValidCycles :: [Asset] -> Bool
prop_pathsAreValidCycles assets =
    all isValidCycle (allTriangularPaths dedupAssets)
  where
    dedupAssets = nub assets
    isValidCycle p =
        quote (arbPair1 p) == base (arbPair2 p) &&
        quote (arbPair2 p) == base (arbPair3 p) &&
        quote (arbPair3 p) == base (arbPair1 p)

-- Una oportunidad con ganancia >= umbral es detectada por makeDecision.
-- Si hay al menos una oportunidad que supera el umbral, el resultado es DoTrade.
prop_decisionPicksWhenProfitable :: Property
prop_decisionPicksWhenProfitable =
    forAll genProfitableOpp $ \(threshold, opp) ->
        case makeDecision (ProfitPct threshold) [opp] of
            DoTrade picked -> arbProfitPerc picked >= ProfitPct threshold
            NoTrade        -> False
  where
    genProfitableOpp = do
        asset    <- arbitrary
        amtIn    <- choose (0.001, 100.0)
        profit   <- choose (0.01, 10.0)
        threshold <- choose (0, profit)    -- umbral bajo el profit
        let amtOut = amtIn * (1 + profit / 100)
            (a, b, c) = (BTC, ETH, USDT)
            path = case mkTriangularPath (Pair a b) (Pair b c) (Pair c a) of
                     Just p  -> p
                     Nothing -> error "camino de test inválido"
            opp  = ArbOpportunity path (AssetQty asset amtIn) (AssetQty asset amtOut)
        return (threshold, opp)

-- assetUsdtRateWhenSelling USDT siempre devuelve Right 1.0
prop_usdtRateIsOne :: MarketSnapshot -> Bool
prop_usdtRateIsOne snap =
    assetUsdtRateWhenSelling snap USDT == Right (UsdtRate 1.0) &&
    assetUsdtRateWhenBuying  snap USDT == Right (UsdtRate 1.0)

instance Arbitrary MarketSnapshot where
    arbitrary = do
        entries <- listOf $ do
            b <- arbitrary
            q <- arbitrary `suchThat` (/= b)
            pq <- arbitrary
            return (Pair b q, pq)
        return $ MarketSnapshot (Map.fromList entries)

-- ---------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "=== Tests de propiedades del núcleo puro ==="
    putStrLn ""
    putStrLn "Prop 1: makeDecision [] == NoTrade"
    quickCheck prop_noTradeOnEmpty
    putStrLn ""
    putStrLn "Prop 2: allTriangularPaths genera solo ciclos válidos"
    quickCheck prop_pathsAreValidCycles
    putStrLn ""
    putStrLn "Prop 3: oportunidad sobre umbral → DoTrade"
    quickCheck prop_decisionPicksWhenProfitable
    putStrLn ""
    putStrLn "Prop 4: tasa USDT→USDT siempre es 1.0"
    quickCheck prop_usdtRateIsOne
