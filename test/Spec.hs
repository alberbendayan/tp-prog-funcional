{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Test.QuickCheck
import qualified Data.Map.Strict as Map
import Data.List (nub)

import Bot.Domain
import Bot.Arbitraje
import Bot.Pricing
import Bot.Runtime (netPnlUsdtFromDeltas)


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

instance Arbitrary MarketSnapshot where
    arbitrary = do
        entries <- listOf $ do
            b <- arbitrary
            q <- arbitrary `suchThat` (/= b)
            pq <- arbitrary
            return (Pair b q, pq)
        return $ MarketSnapshot (Map.fromList entries)

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

-- Si todas las oportunidades están bajo el umbral, makeDecision no opera.
-- Evita ejecutar cuando ninguna oportunidad es rentable según el criterio configurado.
prop_noTradeWhenAllBelowThreshold :: Property
prop_noTradeWhenAllBelowThreshold =
    forAll genBelowThresholdOpps $ \(threshold, opps) ->
        makeDecision threshold opps == NoTrade
  where
    genBelowThresholdOpps = do
        threshold <- choose (0.01, 10.0)
        opps      <- listOf (genOppBelow threshold)
        return (ProfitPct threshold, opps)
    genOppBelow threshold = do
        amtIn  <- choose (0.001, 100.0)
        profit <- choose (0.0, max 0 (threshold - 0.001))
        let amtOut = amtIn * (1 + profit / 100)
            path   = case mkTriangularPath (Pair BTC ETH) (Pair ETH USDT) (Pair USDT BTC) of
                       Just p  -> p
                       Nothing -> error "camino de test inválido"
        return $ ArbOpportunity path (AssetQty BTC amtIn) (AssetQty BTC amtOut)

-- assetUsdtRateWhenSelling USDT siempre devuelve Right 1.0
prop_usdtRateIsOne :: MarketSnapshot -> Bool
prop_usdtRateIsOne snap =
    assetUsdtRateWhenSelling snap USDT == Right (UsdtRate 1.0) &&
    assetUsdtRateWhenBuying  snap USDT == Right (UsdtRate 1.0)

approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) <= 1e-9 * (1 + abs a + abs b)

rightApprox :: Either e Double -> Double -> Bool
rightApprox (Right x) y = approxEq x y
rightApprox _         _ = False

mkSnap :: [(Pair, PairQuote)] -> MarketSnapshot
mkSnap = MarketSnapshot . Map.fromList

genAssetQuote :: Gen (Asset, PairQuote, Double)
genAssetQuote = do
    asset <- elements [BTC, ETH, BNB]
    pq    <- arbitrary
    q     <- choose (0.001, 1000)
    return (asset, pq, q)

-- La conversión a USDT usa el precio bid para activos recibidos
-- (delta positivo) y el precio ask para activos entregados (delta negativo).
prop_pnlUsesBidForReceivedAskForDelivered :: Property
prop_pnlUsesBidForReceivedAskForDelivered =
    forAll genAssetQuote $ \(asset, pq, q) ->
        let snap      = mkSnap [(Pair asset USDT, pq)]
            bid       = unPrice (bidPrice pq)
            ask       = unPrice (askPrice pq)
            received  = netPnlUsdtFromDeltas snap (Map.singleton asset q)
            delivered = netPnlUsdtFromDeltas snap (Map.singleton asset (negate q))
        in rightApprox received  (q * bid) &&
           rightApprox delivered (negate q * ask)

-- Por la asimetría bid <= ask, recibir y luego entregar la misma
-- cantidad de un activo nunca produce ganancia: el spread siempre cuesta.
prop_spreadNeverProfits :: Property
prop_spreadNeverProfits =
    forAll genAssetQuote $ \(asset, pq, q) ->
        let snap      = mkSnap [(Pair asset USDT, pq)]
            received  = netPnlUsdtFromDeltas snap (Map.singleton asset q)
            delivered = netPnlUsdtFromDeltas snap (Map.singleton asset (negate q))
        in case (received, delivered) of
             (Right r, Right d) -> r + d <= 1e-9
             _                  -> False

-- Snapshot con cotización directa a USDT para BTC, ETH y BNB, junto con un
-- mapa de deltas (multi-activo) sobre esos activos más USDT.
genDeltasWithSnap :: Gen (MarketSnapshot, Map.Map Asset Double)
genDeltasWithSnap = do
    let assets = [BTC, ETH, BNB]
    quotes <- mapM (\a -> do pq <- arbitrary; return (Pair a USDT, pq)) assets
    ds     <- mapM (\a -> do d <- choose (-1000, 1000); return (a, d)) (USDT : assets)
    return (mkSnap quotes, Map.fromList ds)

-- El PnL en USDT de un mapa multi-activo es la suma de los PnL de
-- cada activo convertido por separado (aditividad sobre los deltas).
prop_pnlIsAdditiveOverAssets :: Property
prop_pnlIsAdditiveOverAssets =
    forAll genDeltasWithSnap $ \(snap, deltas) ->
        let whole = netPnlUsdtFromDeltas snap deltas
            parts = traverse (\(a, d) -> netPnlUsdtFromDeltas snap (Map.singleton a d))
                             (Map.toList deltas)
        in case (whole, fmap sum parts) of
             (Right w, Right p) -> approxEq w p
             _                  -> False

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
    putStrLn ""
    putStrLn "Prop 5: todas bajo umbral → NoTrade"
    quickCheck prop_noTradeWhenAllBelowThreshold
    putStrLn ""
    putStrLn "Prop 6: PnL usa bid para recibidos y ask para entregados"
    quickCheck prop_pnlUsesBidForReceivedAskForDelivered
    putStrLn ""
    putStrLn "Prop 7: el spread nunca produce ganancia (ida y vuelta)"
    quickCheck prop_spreadNeverProfits
    putStrLn ""
    putStrLn "Prop 8: PnL multi-activo es aditivo sobre los deltas"
    quickCheck prop_pnlIsAdditiveOverAssets
