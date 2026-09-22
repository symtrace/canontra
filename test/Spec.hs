{- |
Module      : Main
Description : Test suite runner for canontra v0.0.4-alpha.

This test runner aggregates parser tests, property-based tests,
scope and symbol analysis tests, call graph tests, diff diagnostic tests,
polyglot language tests, control-flow graph tests, data-flow graph tests,
post-v0.0.3 bugfix regressions, optimization engines, and transformation fixture validations.
-}
module Main (main) where

import Test.Hspec

import qualified Canontra.BugfixSpec as BugfixSpec
import qualified Canontra.CallGraphSpec as CallGraphSpec
import qualified Canontra.CFGSpec as CFGSpec
import qualified Canontra.ConformanceSpec as ConformanceSpec
import qualified Canontra.DFGSpec as DFGSpec
import qualified Canontra.DiffSpec as DiffSpec
import qualified Canontra.FastScanSpec as FastScanSpec
import qualified Canontra.FixtureSpec as FixtureSpec
import qualified Canontra.GraphSoundnessSpec as GraphSoundnessSpec
import qualified Canontra.MerkleCacheV3Spec as MerkleCacheV3Spec
import qualified Canontra.NormalizeSpec as NormalizeSpec
import qualified Canontra.OptimSpec as OptimSpec
import qualified Canontra.OutlineSpec as OutlineSpec
import qualified Canontra.ParserSpec as ParserSpec
import qualified Canontra.PolyglotSpec as PolyglotSpec
import qualified Canontra.PropertySpec as PropertySpec
import qualified Canontra.ScopeSpec as ScopeSpec
import qualified Canontra.SymbolTableSpec as SymbolTableSpec
import qualified Canontra.WholeRepoGraphSpec as WholeRepoGraphSpec
import qualified Canontra.ImpactAnalysisSpec as ImpactAnalysisSpec
import qualified Canontra.TypeContractSpec as TypeContractSpec
import qualified Canontra.PagedCacheSpec as PagedCacheSpec
import qualified Canontra.WatcherSpec as WatcherSpec
import qualified Canontra.SecuritySpec as SecuritySpec
import qualified Canontra.ExportSpec as ExportSpec
import qualified Canontra.CLISpec as CLISpec
import qualified Canontra.MetamorphicSpec as MetamorphicSpec

main :: IO ()
main = hspec $ do
  describe "Canontra.Parser" ParserSpec.spec
  describe "Canontra.SymbolTable" SymbolTableSpec.spec
  describe "Canontra.Polyglot" PolyglotSpec.spec
  describe "Canontra.Outline" OutlineSpec.spec
  describe "Canontra.Properties" PropertySpec.spec
  describe "Canontra.Normalize" NormalizeSpec.spec
  describe "Canontra.Scope" ScopeSpec.spec
  describe "Canontra.CallGraph" CallGraphSpec.spec
  describe "Canontra.CFG" CFGSpec.spec
  describe "Canontra.DFG" DFGSpec.spec
  describe "Canontra.Diff" DiffSpec.spec
  describe "Canontra.Bugfix" BugfixSpec.spec
  describe "Canontra.Optim" OptimSpec.spec
  describe "Canontra.FastScan" FastScanSpec.spec
  describe "Canontra.MerkleCacheV3" MerkleCacheV3Spec.spec
  describe "Canontra.PagedCache" PagedCacheSpec.spec
  describe "Canontra.Watcher" WatcherSpec.spec
  describe "Canontra.Conformance" ConformanceSpec.spec
  describe "Canontra.GraphSoundness" GraphSoundnessSpec.spec
  describe "Canontra.WholeRepoGraph" WholeRepoGraphSpec.spec
  describe "Canontra.ImpactAnalysis" ImpactAnalysisSpec.spec
  describe "Canontra.TypeContract" TypeContractSpec.spec
  describe "Canontra.Security" SecuritySpec.spec
  describe "Canontra.Export" ExportSpec.spec
  describe "Canontra.CLI" CLISpec.spec
  describe "Canontra.Metamorphic" MetamorphicSpec.spec
  describe "Canontra.Fixtures" FixtureSpec.spec
