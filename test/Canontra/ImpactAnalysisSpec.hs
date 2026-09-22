{- |
Module      : Canontra.ImpactAnalysisSpec
Description : Test specification for semantic change impact analysis and invalidation slicing.

Validates 3-tier severity classification, minimal transitive invalidation slicing,
circular call graph termination, and JSON manifest generation for CI/CD test runners.
-}
{-# LANGUAGE OverloadedStrings #-}
module Canontra.ImpactAnalysisSpec (spec) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Canontra.Analysis.Impact
import Canontra.Analysis.WholeRepoGraph (buildWholeRepoCallGraph)
import Canontra.Fingerprint.Bundle (computeBundle)
import Canontra.Parser.Polyglot (parsePolyglotSource)

spec :: Spec
spec = do
  describe "Semantic Change Severity Classification" $ do
    let authBase = T.unlines
          [ "def verify(token: str) -> bool:"
          , "    return len(token) > 10"
          ]
    let authTrivia = T.unlines
          [ "# Formatted version with comment"
          , "def verify( token: str ) -> bool:"
          , "    \"\"\"Docstring\"\"\""
          , "    return len(token) > 10"
          ]
    let authLogic = T.unlines
          [ "def verify(token: str) -> bool:"
          , "    return len(token) > 20"
          ]
    let authInterface = T.unlines
          [ "def verify(token: str, secret: str) -> bool:"
          , "    return len(token) > 10 and secret == 'admin'"
          ]
    let authDep = T.unlines
          [ "import hashlib"
          , ""
          , "def verify(token: str) -> bool:"
          , "    return len(token) > 10"
          ]

    it "classifies formatting and comment edits as SeverityTrivia" $ do
      case ( computeBundle "auth.py" (TE.encodeUtf8 authBase) authBase
           , computeBundle "auth.py" (TE.encodeUtf8 authTrivia) authTrivia
           ) of
        (Right b1, Right b2) -> classifySeverity b1 b2 `shouldBe` SeverityTrivia
        _ -> expectationFailure "Bundle computation failed"

    it "classifies internal function body mutations as SeverityInternalLogic" $ do
      case ( computeBundle "auth.py" (TE.encodeUtf8 authBase) authBase
           , computeBundle "auth.py" (TE.encodeUtf8 authLogic) authLogic
           ) of
        (Right b1, Right b2) -> classifySeverity b1 b2 `shouldBe` SeverityInternalLogic
        _ -> expectationFailure "Bundle computation failed"

    it "classifies parameter and signature mutations as SeverityInterface" $ do
      case ( computeBundle "auth.py" (TE.encodeUtf8 authBase) authBase
           , computeBundle "auth.py" (TE.encodeUtf8 authInterface) authInterface
           ) of
        (Right b1, Right b2) -> classifySeverity b1 b2 `shouldBe` SeverityInterface
        _ -> expectationFailure "Bundle computation failed"

    it "classifies import additions as SeverityDependency" $ do
      case ( computeBundle "auth.py" (TE.encodeUtf8 authBase) authBase
           , computeBundle "auth.py" (TE.encodeUtf8 authDep) authDep
           ) of
        (Right b1, Right b2) -> classifySeverity b1 b2 `shouldBe` SeverityDependency
        _ -> expectationFailure "Bundle computation failed"

  describe "Minimal Invalidation Slicing (CIA)" $ do
    let authSrc = T.unlines
          [ "def authenticate(user, key):"
          , "    return user == 'root'"
          ]
    let authSrcTrivia = T.unlines
          [ "# whitespace change"
          , "def authenticate( user , key ) :"
          , "    return user == 'root'"
          ]
    let authSrcLogic = T.unlines
          [ "def authenticate(user, key):"
          , "    return user == 'admin' or user == 'root'"
          ]
    let authSrcInterface = T.unlines
          [ "def authenticate(user, key, realm):"
          , "    return user == 'root'"
          ]
    let appSrc = T.unlines
          [ "import auth"
          , ""
          , "def handle(u, k):"
          , "    return auth.authenticate(u, k)"
          ]
    let gatewaySrc = T.unlines
          [ "import app"
          , ""
          , "def dispatch(u, k):"
          , "    return app.handle(u, k)"
          ]
    let utilsSrc = T.unlines
          [ "def helper():"
          , "    return 42"
          ]

    let allFiles =
          [ "auth.py"
          , "app.py"
          , "gateway.py"
          , "utils.py"
          , "tests/test_auth.py"
          , "tests/test_app.py"
          , "tests/test_gateway.py"
          , "tests/test_utils.py"
          ]

    it "yields 0 downstream invalidations and 100% saved compute for trivia edits" $ do
      case ( parsePolyglotSource "auth.py" authSrc
           , parsePolyglotSource "app.py" appSrc
           , parsePolyglotSource "gateway.py" gatewaySrc
           , parsePolyglotSource "utils.py" utilsSrc
           , computeBundle "auth.py" (TE.encodeUtf8 authSrc) authSrc
           , computeBundle "auth.py" (TE.encodeUtf8 authSrcTrivia) authSrcTrivia
           ) of
        (Right pAuth, Right pApp, Right pGw, Right pUt, Right bOld, Right bNew) -> do
          let modules = [("auth.py", pAuth), ("app.py", pApp), ("gateway.py", pGw), ("utils.py", pUt)]
              wcg = buildWholeRepoCallGraph modules
              slice = computeImpactSlice "auth.py" bOld bNew wcg allFiles
          impactSeverity slice `shouldBe` SeverityTrivia
          impactTransitiveFiles slice `shouldBe` []
          impactInvalidatedTests slice `shouldBe` []
          impactSavedComputePct slice `shouldBe` 100.0
        _ -> expectationFailure "Setup failed in trivia test"

    it "invalidates only local unit tests when internal body logic changes" $ do
      case ( parsePolyglotSource "auth.py" authSrc
           , parsePolyglotSource "app.py" appSrc
           , parsePolyglotSource "gateway.py" gatewaySrc
           , parsePolyglotSource "utils.py" utilsSrc
           , computeBundle "auth.py" (TE.encodeUtf8 authSrc) authSrc
           , computeBundle "auth.py" (TE.encodeUtf8 authSrcLogic) authSrcLogic
           ) of
        (Right pAuth, Right pApp, Right pGw, Right pUt, Right bOld, Right bNew) -> do
          let modules = [("auth.py", pAuth), ("app.py", pApp), ("gateway.py", pGw), ("utils.py", pUt)]
              wcg = buildWholeRepoCallGraph modules
              slice = computeImpactSlice "auth.py" bOld bNew wcg allFiles
          impactSeverity slice `shouldBe` SeverityInternalLogic
          impactTransitiveFiles slice `shouldBe` ["auth.py"]
          impactInvalidatedTests slice `shouldBe` ["tests/test_auth.py"]
          "tests/test_app.py" `elem` impactInvalidatedTests slice `shouldBe` False
          impactSavedComputePct slice `shouldSatisfy` (> 80.0)
        _ -> expectationFailure "Setup failed in internal logic test"

    it "transitively invalidates all downstream callers when public interface changes" $ do
      case ( parsePolyglotSource "auth.py" authSrc
           , parsePolyglotSource "app.py" appSrc
           , parsePolyglotSource "gateway.py" gatewaySrc
           , parsePolyglotSource "utils.py" utilsSrc
           , computeBundle "auth.py" (TE.encodeUtf8 authSrc) authSrc
           , computeBundle "auth.py" (TE.encodeUtf8 authSrcInterface) authSrcInterface
           ) of
        (Right pAuth, Right pApp, Right pGw, Right pUt, Right bOld, Right bNew) -> do
          let modules = [("auth.py", pAuth), ("app.py", pApp), ("gateway.py", pGw), ("utils.py", pUt)]
              wcg = buildWholeRepoCallGraph modules
              slice = computeImpactSlice "auth.py" bOld bNew wcg allFiles
          impactSeverity slice `shouldBe` SeverityInterface
          "auth.py" `elem` impactTransitiveFiles slice `shouldBe` True
          "app.py" `elem` impactTransitiveFiles slice `shouldBe` True
          "gateway.py" `elem` impactTransitiveFiles slice `shouldBe` True
          "utils.py" `elem` impactTransitiveFiles slice `shouldBe` False
          "tests/test_utils.py" `elem` impactInvalidatedTests slice `shouldBe` False
        _ -> expectationFailure "Setup failed in interface test"

  describe "Circular Dependency Invalidation Termination" $ do
    let srvA = T.unlines
          [ "import srv_b"
          , "def call_a(x):"
          , "    return srv_b.call_b(x)"
          ]
    let srvB = T.unlines
          [ "import srv_a"
          , "def call_b(x):"
          , "    return srv_a.call_a(x)"
          ]
    let srvAMutated = T.unlines
          [ "import srv_b"
          , "def call_a(x, y):"
          , "    return srv_b.call_b(x)"
          ]

    it "terminates cleanly without cycle loop when traversing Tarjan SCC cycles" $ do
      case ( parsePolyglotSource "srv_a.py" srvA
           , parsePolyglotSource "srv_b.py" srvB
           , computeBundle "srv_a.py" (TE.encodeUtf8 srvA) srvA
           , computeBundle "srv_a.py" (TE.encodeUtf8 srvAMutated) srvAMutated
           ) of
        (Right pA, Right pB, Right bOld, Right bNew) -> do
          let modules = [("srv_a.py", pA), ("srv_b.py", pB)]
              wcg = buildWholeRepoCallGraph modules
              slice = computeImpactSlice "srv_a.py" bOld bNew wcg ["srv_a.py", "srv_b.py"]
          impactSeverity slice `shouldBe` SeverityInterface
          "srv_a.py" `elem` impactTransitiveFiles slice `shouldBe` True
          "srv_b.py" `elem` impactTransitiveFiles slice `shouldBe` True
        _ -> expectationFailure "Setup failed in circular test"

  describe "JSON Impact Manifest Format" $ do
    let slice = ImpactSlice
          { impactTargetFile       = "core/auth.py"
          , impactSeverity         = SeverityInterface
          , impactDirectCallers    = []
          , impactTransitiveFiles  = ["core/auth.py", "api/handler.py"]
          , impactInvalidatedTests = ["tests/test_auth.py", "tests/test_handler.py"]
          , impactSavedComputePct  = 85.5
          }

    it "serializes and roundtrips to valid JSON" $ do
      let jsonText = formatImpactSliceJson slice
          mDecoded = Aeson.decode (BL.fromStrict (TE.encodeUtf8 jsonText)) :: Maybe ImpactSlice
      mDecoded `shouldBe` Just slice

  describe "Test Discovery & Compute Savings Calculations" $ do
    it "discovers test files matching affected source basenames" $ do
      let affected = ["src/auth/jwt.py", "src/payment/stripe.py"]
          allFiles =
            [ "src/auth/jwt.py"
            , "src/payment/stripe.py"
            , "tests/test_jwt.py"
            , "tests/test_stripe.py"
            , "tests/test_database.py"
            , "src/database/sql.py"
            ]
          tests = findMatchingTests affected allFiles
      "tests/test_jwt.py" `elem` tests `shouldBe` True
      "tests/test_stripe.py" `elem` tests `shouldBe` True
      "tests/test_database.py" `elem` tests `shouldBe` False

    it "discovers TypeScript spec files matching .spec.ts" $ do
      let affected = ["src/userService.ts"]
          allFiles = ["src/userService.ts", "test/userService.spec.ts", "test/authService.spec.ts"]
          tests = findMatchingTests affected allFiles
      tests `shouldBe` ["test/userService.spec.ts"]

    it "discovers Go test files matching _test.go" $ do
      let affected = ["pkg/math/calc.go"]
          allFiles = ["pkg/math/calc.go", "pkg/math/calc_test.go", "pkg/net/http_test.go"]
          tests = findMatchingTests affected allFiles
      tests `shouldBe` ["pkg/math/calc_test.go"]

    it "computes 100% saved compute when 0 files are invalidated" $ do
      let allFiles = ["a.py", "b.py", "c.py", "d.py"]
          saved = computeSavedPct [] allFiles
      saved `shouldBe` 100.0

    it "computes 0% saved compute when all files are invalidated" $ do
      let allFiles = ["a.py", "b.py"]
          saved = computeSavedPct allFiles allFiles
      saved `shouldBe` 0.0

    it "computes 75% saved compute when 1 of 4 files is affected" $ do
      let allFiles = ["a.py", "b.py", "c.py", "d.py"]
          saved = computeSavedPct ["a.py"] allFiles
      saved `shouldBe` 75.0

    it "formats human-readable diagnostic report for SeverityTrivia" $ do
      let sliceTrivia = ImpactSlice "util.py" SeverityTrivia [] [] [] 100.0
          report = formatImpactSlice sliceTrivia
      T.isInfixOf "CANONTRA SEMANTIC CHANGE IMPACT ANALYSIS" report `shouldBe` True
      T.isInfixOf "LEVEL 1: TRIVIA" report `shouldBe` True
      T.isInfixOf "Safe to skip CI build" report `shouldBe` True

    it "formats human-readable diagnostic report for SeverityInternalLogic" $ do
      let sliceLogic = ImpactSlice "util.py" SeverityInternalLogic [] ["util.py"] ["test_util.py"] 80.0
          report = formatImpactSlice sliceLogic
      T.isInfixOf "LEVEL 2: INTERNAL LOGIC" report `shouldBe` True
      T.isInfixOf "Run targeted local unit tests only" report `shouldBe` True

    it "formats human-readable diagnostic report for SeverityInterface" $ do
      let sliceInterface = ImpactSlice "api.py" SeverityInterface [] ["api.py", "client.py"] ["test_api.py", "test_client.py"] 50.0
          report = formatImpactSlice sliceInterface
      T.isInfixOf "LEVEL 3: PUBLIC INTERFACE" report `shouldBe` True
      T.isInfixOf "Run transitive test slice" report `shouldBe` True

    it "formats human-readable diagnostic report for SeverityDependency" $ do
      let sliceDep = ImpactSlice "lib.py" SeverityDependency [] ["lib.py"] ["test_lib.py"] 90.0
          report = formatImpactSlice sliceDep
      T.isInfixOf "LEVEL 3: DEPENDENCY" report `shouldBe` True
      T.isInfixOf "Run transitive dependency slice" report `shouldBe` True

    it "does not invalidate unrelated disconnected modules" $ do
      let modA = "def a(): return 1\n"
          modB = "def b(): return 2\n"
          modAMut = "def a(x): return x\n"
      case ( parsePolyglotSource "a.py" modA
           , parsePolyglotSource "b.py" modB
           , computeBundle "a.py" (TE.encodeUtf8 modA) modA
           , computeBundle "a.py" (TE.encodeUtf8 modAMut) modAMut
           ) of
        (Right pA, Right pB, Right bOld, Right bNew) -> do
          let wcg = buildWholeRepoCallGraph [("a.py", pA), ("b.py", pB)]
              slice = computeImpactSlice "a.py" bOld bNew wcg ["a.py", "b.py"]
          "b.py" `elem` impactTransitiveFiles slice `shouldBe` False
        _ -> expectationFailure "Setup failed"

    it "invalidates all callers when a shared dependency interface changes" $ do
      let shared = "def helper(): return 1\n"
          sharedMut = "def helper(arg): return arg\n"
          caller1 = "import shared\ndef call1(): return shared.helper()\n"
          caller2 = "import shared\ndef call2(): return shared.helper()\n"
      case ( parsePolyglotSource "shared.py" shared
           , parsePolyglotSource "c1.py" caller1
           , parsePolyglotSource "c2.py" caller2
           , computeBundle "shared.py" (TE.encodeUtf8 shared) shared
           , computeBundle "shared.py" (TE.encodeUtf8 sharedMut) sharedMut
           ) of
        (Right pS, Right pC1, Right pC2, Right bOld, Right bNew) -> do
          let wcg = buildWholeRepoCallGraph [("shared.py", pS), ("c1.py", pC1), ("c2.py", pC2)]
              slice = computeImpactSlice "shared.py" bOld bNew wcg ["shared.py", "c1.py", "c2.py"]
          "c1.py" `elem` impactTransitiveFiles slice `shouldBe` True
          "c2.py" `elem` impactTransitiveFiles slice `shouldBe` True
        _ -> expectationFailure "Setup failed"
