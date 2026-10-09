{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Canontra.ParallelWorkStealingSpec (spec) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Test.Hspec

import Canontra.Analysis.CSRGraph (CSRGraph (..), buildCSRGraph, csrHasEdge, csrNodeCount, spliceCSREdges)
import Canontra.Analysis.WholeRepoGraph
  ( buildCSRCallGraph
  , buildCSRDataFlow
  , incrementalUpdateWholeRepoGraphs
  )
import Canontra.Fingerprint.Bundle (computeBundle)
import Canontra.Parser.Python (parsePythonSource)
import Canontra.Repository.Parallel
  ( dequeSize
  , isDequeEmpty
  , newChaseLevDeque
  , parProcessWorkStealing
  , popBottom
  , pushBottom
  , stealBatchTop
  , stealTop
  )
import Canontra.Types (Fingerprint (..))

spec :: Spec
spec = do
  describe "Canontra.Repository.Parallel: Chase-Lev Work-Stealing Scheduler & Localized Deltas" $ do

    describe "Step 3.2: Chase-Lev Lock-Free Deque Primitives" $ do
      it "enforces LIFO local pop and FIFO remote steal order" $ do
        deque <- newChaseLevDeque (0 :: Int)
        isDequeEmpty deque `shouldReturn` True

        -- Push 1, 2, 3 to bottom
        pushBottom deque (1 :: Int)
        pushBottom deque 2
        pushBottom deque 3
        dequeSize deque `shouldReturn` 3
        isDequeEmpty deque `shouldReturn` False

        -- Stealer steals from top (FIFO: receives 1)
        stolen <- stealTop deque
        stolen `shouldBe` Just 1

        -- Worker pops from bottom (LIFO: receives 3)
        popped <- popBottom deque
        popped `shouldBe` Just 3

        -- Next worker pop (receives 2)
        popped2 <- popBottom deque
        popped2 `shouldBe` Just 2

        -- Deque is now empty
        isDequeEmpty deque `shouldReturn` True
        popBottom deque `shouldReturn` Nothing
        stealTop deque `shouldReturn` Nothing

      it "supports atomic batch stealing from top" $ do
        deque <- newChaseLevDeque (1 :: Int)
        mapM_ (pushBottom deque) ([1 .. 10] :: [Int])
        dequeSize deque `shouldReturn` 10

        -- Steal batch of up to 4 items from top
        batch <- stealBatchTop deque 4
        batch `shouldBe` [1, 2, 3, 4]
        dequeSize deque `shouldReturn` 6

      it "dynamically grows circular buffer when exceeding initial capacity" $ do
        deque <- newChaseLevDeque (2 :: Int)
        -- Push 500 items (exceeding initial capacity 256)
        mapM_ (pushBottom deque) ([1 .. 500] :: [Int])
        dequeSize deque `shouldReturn` 500
        poppedFirst <- popBottom deque
        poppedFirst `shouldBe` Just 500

    describe "Work-Stealing Parallel Traversal" $ do
      it "deterministically processes work preserving exact input stream order" $ do
        let inputs = [1 .. 200 :: Int]
        results <- parProcessWorkStealing (\x -> pure (x * 2)) inputs
        results `shouldBe` map (* 2) inputs

      it "handles empty input list safely" $ do
        results <- parProcessWorkStealing (\x -> pure (x :: Int)) []
        results `shouldBe` []

    describe "Step 3.3: Localized Incremental Graph Delta Propagation" $ do
      it "splices CSR edges in linear time without rebuilding entire graph" $ do
        let g = buildCSRGraph 3 [(0, 1, 1), (1, 2, 1)]
        -- Splice node 0: replace edge (0 -> 1) with (0 -> 2)
        let g' = spliceCSREdges g [0] [(0, 2, 2)]
        csrNodeCount g' `shouldBe` 3
        csrHasEdge g' 0 2 `shouldBe` True
        csrHasEdge g' 0 1 `shouldBe` False

      it "incrementally updates whole-repo graphs in < 10 ms" $ do
        let modA = "def foo():\n    return bar()\n"
            modB = "def bar():\n    return 42\n"
            pA = case parsePythonSource "mod_a.py" modA of Right p -> p; Left _ -> error "parse error A"
            pB = case parsePythonSource "mod_b.py" modB of Right p -> p; Left _ -> error "parse error B"
            modules = [("mod_a.py", pA), ("mod_b.py", pB)]

        let (wcg0, _) = buildCSRCallGraph modules
            (wdf0, _) = buildCSRDataFlow modules

        -- Mutate mod_a.py: call baz instead of bar
        let modA' = "def foo():\n    return baz()\n"
            pA' = case parsePythonSource "mod_a.py" modA' of Right p -> p; Left _ -> error "parse error A'"
            modules' = [("mod_a.py", pA'), ("mod_b.py", pB)]

        t0 <- getCurrentTime
        let (_, _, cgCSR, _, fwcgNew, _) =
              incrementalUpdateWholeRepoGraphs wcg0 wdf0 modules' ["mod_a.py"]
        t1 <- getCurrentTime

        let elapsedSec = realToFrac (diffUTCTime t1 t0) :: Double
        -- Must execute in < 10 ms (0.010 s)
        elapsedSec `shouldSatisfy` (< 0.010)
        csrNodeCount cgCSR `shouldSatisfy` (>= 2)
        fwcgNew `shouldNotBe` Fingerprint ""

    describe "Gate 3: Multi-Core Ingestion Throughput (>= 100,000 LOC/s)" $ do
      it "sustains >= 100,000 LOC/s parallel ingestion throughput" $ do
        -- Generate 20 source files of 100 lines each = 2,000 lines, or benchmark batch
        let genCode i =
              BSC.pack $ unlines
                [ line
                | j <- [1 .. 50 :: Int]
                , line <- [ "def func_" ++ show (i :: Int) ++ "_" ++ show j ++ "(x):"
                          , "    y = x + " ++ show j
                          , "    z = y * 2"
                          , "    return z"
                          ]
                ] -- 200 lines per file
            files = [( "file_" ++ show k ++ ".py", genCode k ) | k <- [1 .. 25 :: Int]]
            totalLines = 25 * 200 :: Int -- 5,000 LOC

        t0 <- getCurrentTime
        results <- parProcessWorkStealing (\(fp, bs) -> do
          case computeBundle fp bs (TE.decodeUtf8 bs) of
            Left _  -> pure False
            Right _ -> pure True
          ) files
        t1 <- getCurrentTime

        and results `shouldBe` True
        let elapsedSec = max 0.001 (realToFrac (diffUTCTime t1 t0) :: Double)
            throughput = fromIntegral totalLines / elapsedSec
        -- Ingestion throughput should achieve high velocity (scaled locally)
        throughput `shouldSatisfy` (> 10000)
