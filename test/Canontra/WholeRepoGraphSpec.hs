{- |
Module      : Canontra.WholeRepoGraphSpec
Description : Test specification for whole-repository inter-module call graph and data-flow synthesis.

Validates cross-module symbol resolution, inter-module call graph edges (F_WCG),
cycle-collapsed SCCs via Tarjan's algorithm, inter-procedural data-flow (F_WDF),
dead symbol identification, and multi-tier whole-repository fingerprint determinism.
-}
{-# LANGUAGE OverloadedStrings #-}
module Canontra.WholeRepoGraphSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Canontra.Analysis.WholeRepoGraph
import Canontra.Fingerprint.WholeRepoCallGraph (computeFWCG)
import Canontra.Fingerprint.WholeRepoDataFlow (computeFWDF)
import Canontra.Parser.Polyglot (parsePolyglotSource)
import Canontra.Repository.Repository (computeWholeRepoBundle)
import Canontra.Types

spec :: Spec
spec = do
  describe "Module Path Canonicalization" $ do
    it "canonicalizes relative file paths into dotted module names" $ do
      filePathToModuleName "auth/jwt.py" `shouldBe` "auth.jwt"
      filePathToModuleName "src/core/math.rs" `shouldBe` "src.core.math"
      filePathToModuleName "api/v1/handler.go" `shouldBe` "api.v1.handler"
      filePathToModuleName "pkg/subpkg/__init__.py" `shouldBe` "pkg.subpkg"
      filePathToModuleName "app\\service\\worker.py" `shouldBe` "app.service.worker"

  describe "Cross-Module Call Graph Synthesis (F_WCG)" $ do
    let authJwtSrc = T.unlines
          [ "def verify_token(token: str) -> bool:"
          , "    return len(token) > 10"
          , ""
          , "def decode_token(token: str):"
          , "    return token"
          , ""
          , "def unused_helper():"
          , "    return 42"
          ]

    let appServiceSrc = T.unlines
          [ "import auth.jwt as jwt"
          , ""
          , "def handle_request(req):"
          , "    valid = jwt.verify_token(req)"
          , "    return valid"
          ]

    it "extracts global symbols with correct module namespaces and kinds" $ do
      case (parsePolyglotSource "auth/jwt.py" authJwtSrc, parsePolyglotSource "app/service.py" appServiceSrc) of
        (Right progJwt, Right progSvc) -> do
          let modules = [("auth/jwt.py", progJwt), ("app/service.py", progSvc)]
              wcg = buildWholeRepoCallGraph modules
              symNames = map symDeclName (wcgNodes wcg)
          "verify_token" `elem` symNames `shouldBe` True
          "decode_token" `elem` symNames `shouldBe` True
          "unused_helper" `elem` symNames `shouldBe` True
          "handle_request" `elem` symNames `shouldBe` True
        (Left e1, _) -> expectationFailure (show e1)
        (_, Left e2) -> expectationFailure (show e2)

    it "resolves cross-module caller-to-callee edges" $ do
      case (parsePolyglotSource "auth/jwt.py" authJwtSrc, parsePolyglotSource "app/service.py" appServiceSrc) of
        (Right progJwt, Right progSvc) -> do
          let modules = [("auth/jwt.py", progJwt), ("app/service.py", progSvc)]
              wcg = buildWholeRepoCallGraph modules
              crossEdges = findCrossModuleEdges wcg
          length crossEdges `shouldSatisfy` (> 0)
          let hasSvcToJwt = any (\e ->
                symDeclName (wceCaller e) == "handle_request" &&
                symModule (wceCaller e) == "app.service" &&
                symDeclName (wceCallee e) == "verify_token" &&
                symModule (wceCallee e) == "auth.jwt" &&
                wceIsCrossMod e
                ) crossEdges
          hasSvcToJwt `shouldBe` True
        (Left e1, _) -> expectationFailure (show e1)
        (_, Left e2) -> expectationFailure (show e2)

    it "identifies dead symbols with zero callers across the entire repository" $ do
      case (parsePolyglotSource "auth/jwt.py" authJwtSrc, parsePolyglotSource "app/service.py" appServiceSrc) of
        (Right progJwt, Right progSvc) -> do
          let modules = [("auth/jwt.py", progJwt), ("app/service.py", progSvc)]
              wcg = buildWholeRepoCallGraph modules
              deadSyms = findDeadSymbols wcg
              deadNames = map symDeclName deadSyms
          "unused_helper" `elem` deadNames `shouldBe` True
          "decode_token" `elem` deadNames `shouldBe` True
          "verify_token" `elem` deadNames `shouldBe` False
        (Left e1, _) -> expectationFailure (show e1)
        (_, Left e2) -> expectationFailure (show e2)

  describe "Cross-Module Recursion & Cycle Detection (Tarjan SCC)" $ do
    let svcA = T.unlines
          [ "import service_b as b"
          , ""
          , "def func_a(n):"
          , "    if n <= 0: return 0"
          , "    return b.func_b(n - 1)"
          ]
    let svcB = T.unlines
          [ "import service_a as a"
          , ""
          , "def func_b(n):"
          , "    if n <= 0: return 0"
          , "    return a.func_a(n - 1)"
          ]

    it "detects cross-module circular call cycles via Tarjan's SCC" $ do
      case (parsePolyglotSource "service_a.py" svcA, parsePolyglotSource "service_b.py" svcB) of
        (Right pA, Right pB) -> do
          let modules = [("service_a.py", pA), ("service_b.py", pB)]
              wcg = buildWholeRepoCallGraph modules
              sccs = findWholeRepoSCCs wcg
              cycles = filter (\c -> length c > 1) sccs
          length cycles `shouldSatisfy` (>= 1)
          let cycleNames = map symDeclName (head cycles)
          "func_a" `elem` cycleNames `shouldBe` True
          "func_b" `elem` cycleNames `shouldBe` True
        (Left e1, _) -> expectationFailure (show e1)
        (_, Left e2) -> expectationFailure (show e2)

  describe "Inter-Procedural Data-Flow Synthesis (F_WDF)" $ do
    let srcCalc = T.unlines
          [ "def compute(x: int, y: int) -> int:"
          , "    return x + y"
          ]
    let srcMain = T.unlines
          [ "import calc"
          , ""
          , "def run_calc(val):"
          , "    res = calc.compute(val, 10)"
          , "    return res"
          ]

    it "synthesizes cross-module argument bindings and return-flow edges" $ do
      case (parsePolyglotSource "calc.py" srcCalc, parsePolyglotSource "main.py" srcMain) of
        (Right pCalc, Right pMain) -> do
          let modules = [("calc.py", pCalc), ("main.py", pMain)]
              wdf = buildWholeRepoDataFlow modules
              edges = wdfEdges wdf
          length edges `shouldSatisfy` (> 0)
          let hasParam0 = any (\e ->
                symDeclName (ipdfSourceSymbol e) == "run_calc" &&
                symDeclName (ipdfTargetSymbol e) == "compute" &&
                ipdfParamIndex e == 0 &&
                not (ipdfIsReturnFlow e)
                ) edges
          let hasReturn = any (\e ->
                symDeclName (ipdfSourceSymbol e) == "compute" &&
                symDeclName (ipdfTargetSymbol e) == "run_calc" &&
                ipdfIsReturnFlow e
                ) edges
          hasParam0 `shouldBe` True
          hasReturn `shouldBe` True
        (Left e1, _) -> expectationFailure (show e1)
        (_, Left e2) -> expectationFailure (show e2)

  describe "Deterministic Hashing & Invariance Theorems" $ do
    let authSrc = T.unlines
          [ "def login(user, pwd):"
          , "    return user == 'admin'"
          ]
    let authMutated = T.unlines
          [ "# Formatted version with comment churn"
          , "def login( user , pwd ) :"
          , "    # Verify credentials"
          , "    \"\"\"Docstring comment\"\"\""
          , "    return user == 'admin'"
          ]
    let appSrc = T.unlines
          [ "import auth"
          , ""
          , "def auth_handler(u, p):"
          , "    return auth.login(u, p)"
          ]

    it "guarantees F_WCG and F_WDF invariance under whitespace, comments, and trivia" $ do
      case ( parsePolyglotSource "auth.py" authSrc
           , parsePolyglotSource "auth.py" authMutated
           , parsePolyglotSource "app.py" appSrc
           ) of
        (Right pAuth1, Right pAuth2, Right pApp) -> do
          let mods1 = [("auth.py", pAuth1), ("app.py", pApp)]
              mods2 = [("auth.py", pAuth2), ("app.py", pApp)]
              fwcg1 = computeFWCG mods1
              fwcg2 = computeFWCG mods2
              fwdf1 = computeFWDF mods1
              fwdf2 = computeFWDF mods2
          fwcg1 `shouldBe` fwcg2
          fwdf1 `shouldBe` fwdf2
        _ -> expectationFailure "Parse failure in invariance test"

    it "sensitively mutates F_WCG when a cross-module target function is changed" $ do
      let appSrcMutated = T.unlines
            [ "import auth"
            , ""
            , "def auth_handler(u, p):"
            , "    return auth.other_func(u, p)"
            ]
      case ( parsePolyglotSource "auth.py" authSrc
           , parsePolyglotSource "app.py" appSrc
           , parsePolyglotSource "app.py" appSrcMutated
           ) of
        (Right pAuth, Right pApp1, Right pApp2) -> do
          let mods1 = [("auth.py", pAuth), ("app.py", pApp1)]
              mods2 = [("auth.py", pAuth), ("app.py", pApp2)]
              fwcg1 = computeFWCG mods1
              fwcg2 = computeFWCG mods2
          fwcg1 `shouldNotBe` fwcg2
        _ -> expectationFailure "Parse failure in sensitivity test"

  describe "WholeRepoBundle Composition" $ do
    it "constructs a valid WholeRepoBundle with distinct orthogonal hashes" $ do
      let srcM1 = "def f(): return 1"
          srcM2 = "import m1\ndef g(): return m1.f()"
      case (parsePolyglotSource "m1.py" srcM1, parsePolyglotSource "m2.py" srcM2) of
        (Right p1, Right p2) -> do
          let progs = [("m1.py", p1), ("m2.py", p2)]
              entries =
                [ FileEntry "m1.py" (FingerprintBundle (Fingerprint "s1") (Fingerprint "st1") (Fingerprint "d1") (Fingerprint "dp1") (Fingerprint "cg1") (Fingerprint "cf1") (Fingerprint "df1") (Fingerprint "t1") (Fingerprint "c1"))
                , FileEntry "m2.py" (FingerprintBundle (Fingerprint "s2") (Fingerprint "st2") (Fingerprint "d2") (Fingerprint "dp2") (Fingerprint "cg2") (Fingerprint "cf2") (Fingerprint "df2") (Fingerprint "t2") (Fingerprint "c2"))
                ]
              wrb = computeWholeRepoBundle progs entries
          unFingerprint (wrbRepositoryHash wrb) `shouldNotBe` ""
          unFingerprint (wrbCallGraph wrb) `shouldNotBe` ""
          unFingerprint (wrbDataFlow wrb) `shouldNotBe` ""
          unFingerprint (wrbComposite wrb) `shouldNotBe` ""
          wrbCallGraph wrb `shouldNotBe` wrbDataFlow wrb
        _ -> expectationFailure "Parse failure in WholeRepoBundle test"

  describe "Polyglot Cross-Module Integration" $ do
    let tsMath = T.unlines
          [ "export function add(a: number, b: number): number {"
          , "    return a + b;"
          , "}"
          ]
    let tsMain = T.unlines
          [ "import { add } from './math';"
          , "export function run(): number {"
          , "    return add(5, 10);"
          , "}"
          ]

    it "resolves cross-module calls in TypeScript" $ do
      case (parsePolyglotSource "math.ts" tsMath, parsePolyglotSource "main.ts" tsMain) of
        (Right pMath, Right pMain) -> do
          let modules = [("math.ts", pMath), ("main.ts", pMain)]
              wcg = buildWholeRepoCallGraph modules
              edges = wcgEdges wcg
          let hasTsCall = any (\e ->
                symDeclName (wceCaller e) == "run" &&
                symDeclName (wceCallee e) == "add"
                ) edges
          hasTsCall `shouldBe` True
        (Left e1, _) -> expectationFailure (show e1)
        (_, Left e2) -> expectationFailure (show e2)

  describe "Extended Whole-Repository Topologies & Determinism" $ do
    it "resolves a 3-hop linear transitive call chain (A -> B -> C)" $ do
      let srcC = "def leaf(): return 100\n"
          srcB = "import mod_c\ndef middle(): return mod_c.leaf()\n"
          srcA = "import mod_b\ndef top(): return mod_b.middle()\n"
      case (parsePolyglotSource "mod_c.py" srcC, parsePolyglotSource "mod_b.py" srcB, parsePolyglotSource "mod_a.py" srcA) of
        (Right pC, Right pB, Right pA) -> do
          let wcg = buildWholeRepoCallGraph [("mod_c.py", pC), ("mod_b.py", pB), ("mod_a.py", pA)]
              edges = wcgEdges wcg
          length edges `shouldSatisfy` (>= 2)
          let hasAB = any (\e -> symDeclName (wceCaller e) == "top" && symDeclName (wceCallee e) == "middle") edges
              hasBC = any (\e -> symDeclName (wceCaller e) == "middle" && symDeclName (wceCallee e) == "leaf") edges
          hasAB `shouldBe` True
          hasBC `shouldBe` True
        _ -> expectationFailure "Parse failed"

    it "resolves diamond dependency calling topology (A -> B, A -> C, B -> D, C -> D)" $ do
      let srcD = "def base(): return 1\n"
          srcB = "import d\ndef left(): return d.base()\n"
          srcC = "import d\ndef right(): return d.base()\n"
          srcA = "import b\nimport c\ndef root(): return b.left() + c.right()\n"
      case (parsePolyglotSource "d.py" srcD, parsePolyglotSource "b.py" srcB, parsePolyglotSource "c.py" srcC, parsePolyglotSource "a.py" srcA) of
        (Right pD, Right pB, Right pC, Right pA) -> do
          let wcg = buildWholeRepoCallGraph [("d.py", pD), ("b.py", pB), ("c.py", pC), ("a.py", pA)]
              crossEdges = findCrossModuleEdges wcg
          length crossEdges `shouldSatisfy` (>= 4)
        _ -> expectationFailure "Parse failed"

    it "detects 3-node cyclic recursion across modules via Tarjan SCC" $ do
      let srcA = "import b\ndef loop_a(n): return b.loop_b(n - 1) if n > 0 else 0\n"
          srcB = "import c\ndef loop_b(n): return c.loop_c(n - 1) if n > 0 else 0\n"
          srcC = "import a\ndef loop_c(n): return a.loop_a(n - 1) if n > 0 else 0\n"
      case (parsePolyglotSource "a.py" srcA, parsePolyglotSource "b.py" srcB, parsePolyglotSource "c.py" srcC) of
        (Right pA, Right pB, Right pC) -> do
          let wcg = buildWholeRepoCallGraph [("a.py", pA), ("b.py", pB), ("c.py", pC)]
              sccs = wcgSCCs wcg
              cycleSCCs = filter (\s -> length s >= 3) sccs
          length cycleSCCs `shouldBe` 1
        _ -> expectationFailure "Parse failed"

    it "handles empty repository gracefully returning empty WCG" $ do
      let wcg = buildWholeRepoCallGraph []
      wcgNodes wcg `shouldBe` []
      wcgEdges wcg `shouldBe` []
      wcgSCCs wcg `shouldBe` []

    it "handles single-file repository without cross-module edges" $ do
      let src = "def hello(): return 1\ndef world(): return hello()\n"
      case parsePolyglotSource "single.py" src of
        Right p -> do
          let wcg = buildWholeRepoCallGraph [("single.py", p)]
          length (wcgNodes wcg) `shouldBe` 2
          findCrossModuleEdges wcg `shouldBe` []
        Left err -> expectationFailure (show err)

    it "handles disconnected independent modules with 0 cross edges" $ do
      let src1 = "def worker1(): return 1\n"
          src2 = "def worker2(): return 2\n"
      case (parsePolyglotSource "w1.py" src1, parsePolyglotSource "w2.py" src2) of
        (Right p1, Right p2) -> do
          let wcg = buildWholeRepoCallGraph [("w1.py", p1), ("w2.py", p2)]
          length (wcgNodes wcg) `shouldBe` 2
          findCrossModuleEdges wcg `shouldBe` []
        _ -> expectationFailure "Parse failed"

    it "tolerates external standard library imports without creating broken nodes" $ do
      let src = "import os\nimport sys\nimport json\ndef run(): return os.path.exists('file')\n"
      case parsePolyglotSource "ext.py" src of
        Right p -> do
          let wcg = buildWholeRepoCallGraph [("ext.py", p)]
          let localNames = map symDeclName (wcgNodes wcg)
          "run" `elem` localNames `shouldBe` True
        Left err -> expectationFailure (show err)

    it "resolves multiple internal callers to the same imported callee" $ do
      let srcLib = "def common(): return 42\n"
          srcApp = "import lib\ndef caller1(): return lib.common()\ndef caller2(): return lib.common()\n"
      case (parsePolyglotSource "lib.py" srcLib, parsePolyglotSource "app.py" srcApp) of
        (Right pLib, Right pApp) -> do
          let wcg = buildWholeRepoCallGraph [("lib.py", pLib), ("app.py", pApp)]
              edges = wcgEdges wcg
              commonCalls = filter (\e -> symDeclName (wceCallee e) == "common") edges
          length commonCalls `shouldBe` 2
        _ -> expectationFailure "Parse failed"

    it "resolves a single caller invoking multiple distinct imported callees" $ do
      let srcA = "def get_x(): return 1\ndef get_y(): return 2\n"
          srcB = "import a\ndef combine(): return a.get_x() + a.get_y()\n"
      case (parsePolyglotSource "a.py" srcA, parsePolyglotSource "b.py" srcB) of
        (Right pA, Right pB) -> do
          let wcg = buildWholeRepoCallGraph [("a.py", pA), ("b.py", pB)]
              edges = wcgEdges wcg
              fromCombine = filter (\e -> symDeclName (wceCaller e) == "combine") edges
          length fromCombine `shouldBe` 2
        _ -> expectationFailure "Parse failed"

    it "guarantees order invariance: permuting module input order yields identical F_WCG" $ do
      let s1 = "def f1(): return 1\n"
          s2 = "import m1\ndef f2(): return m1.f1()\n"
          s3 = "import m2\ndef f3(): return m2.f2()\n"
      case (parsePolyglotSource "m1.py" s1, parsePolyglotSource "m2.py" s2, parsePolyglotSource "m3.py" s3) of
        (Right p1, Right p2, Right p3) -> do
          let modsForward = [("m1.py", p1), ("m2.py", p2), ("m3.py", p3)]
              modsReverse = [("m3.py", p3), ("m2.py", p2), ("m1.py", p1)]
              fwcg1 = computeFWCG modsForward
              fwcg2 = computeFWCG modsReverse
          fwcg1 `shouldBe` fwcg2
        _ -> expectationFailure "Parse failed"

    it "guarantees order invariance: permuting module input order yields identical F_WDF" $ do
      let s1 = "def f1(x): return x\n"
          s2 = "import m1\ndef f2(y): return m1.f1(y)\n"
      case (parsePolyglotSource "m1.py" s1, parsePolyglotSource "m2.py" s2) of
        (Right p1, Right p2) -> do
          let modsA = [("m1.py", p1), ("m2.py", p2)]
              modsB = [("m2.py", p2), ("m1.py", p1)]
              fwdfA = computeFWDF modsA
              fwdfB = computeFWDF modsB
          fwdfA `shouldBe` fwdfB
        _ -> expectationFailure "Parse failed"

    it "Go: resolves package-level functions across separate files" $ do
      let goUtil = "package main\nfunc Helper() int { return 99 }\n"
          goMain = "package main\nfunc Start() int { return Helper() }\n"
      case (parsePolyglotSource "util.go" goUtil, parsePolyglotSource "main.go" goMain) of
        (Right pUtil, Right pMain) -> do
          let wcg = buildWholeRepoCallGraph [("util.go", pUtil), ("main.go", pMain)]
              syms = map symDeclName (wcgNodes wcg)
          "Helper" `elem` syms `shouldBe` True
          "Start" `elem` syms `shouldBe` True
        _ -> expectationFailure "Parse failed"

    it "Rust: resolves functions across multi-file crates" $ do
      let rsMath = "pub fn add(a: i32, b: i32) -> i32 { a + b }\n"
          rsMain = "mod math;\nfn run() -> i32 { math::add(1, 2) }\n"
      case (parsePolyglotSource "math.rs" rsMath, parsePolyglotSource "main.rs" rsMain) of
        (Right pMath, Right pMain) -> do
          let wcg = buildWholeRepoCallGraph [("math.rs", pMath), ("main.rs", pMain)]
              syms = map symDeclName (wcgNodes wcg)
          "add" `elem` syms `shouldBe` True
          "run" `elem` syms `shouldBe` True
        _ -> expectationFailure "Parse failed"

    it "sensitively alters F_WDF when an inter-procedural argument expression changes" $ do
      let sA = "def compute(x): return x * 2\n"
          sB1 = "import a\ndef run(arg): return a.compute(arg)\n"
          sB2 = "import a\ndef run(): return a.compute(10)\n"
      case (parsePolyglotSource "a.py" sA, parsePolyglotSource "b.py" sB1, parsePolyglotSource "b.py" sB2) of
        (Right pA, Right pB1, Right pB2) -> do
          let fwdf1 = computeFWDF [("a.py", pA), ("b.py", pB1)]
              fwdf2 = computeFWDF [("a.py", pA), ("b.py", pB2)]
          fwdf1 `shouldNotBe` fwdf2
        _ -> expectationFailure "Parse failed"
