{- |
Module      : Canontra.TypeContractSpec
Description : Test specification for Polyglot Flow-Sensitive Structural Type Invariance (F_T).

Validates method permutation invariance, union and intersection type commutativity,
cross-language structural subtyping, nominal interface independence, and mutation sensitivity.
-}
{-# LANGUAGE OverloadedStrings #-}
module Canontra.TypeContractSpec (spec) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Test.Hspec

import Canontra.Analysis.Impact (ChangeSeverity (..), classifySeverity)
import Canontra.Analysis.TypeContract
import Canontra.Comparison.Compare (compareBundles, compareFingerprints, formatComparisonResult)
import Canontra.Fingerprint.Bundle (computeBundleFromSource, computeProgramFingerprints)
import Canontra.Fingerprint.TypeContract (computeFT)
import Canontra.Parser.Polyglot (parsePolyglotSource)
import Canontra.Types

spec :: Spec
spec = do
  describe "Union & Intersection Type Commutativity" $ do
    it "guarantees union commutativity (A | B == B | A)" $ do
      let u1 = parseTypeString "number | string"
          u2 = parseTypeString "string | number"
      u1 `shouldBe` u2

    it "guarantees multi-member union associativity and deduplication" $ do
      let u1 = parseTypeString "bool | number | string | number"
          u2 = parseTypeString "string | bool | number"
      u1 `shouldBe` u2

    it "guarantees intersection commutativity (A & B == B & A)" $ do
      let i1 = parseTypeString "Serializable & Cloneable"
          i2 = parseTypeString "Cloneable & Serializable"
      i1 `shouldBe` i2

  describe "Method Permutation Invariance" $ do
    let tsIfaceOrderA = T.unlines
          [ "export interface DataService {"
          , "    read(key: string): Uint8Array;"
          , "    write(key: string, val: Uint8Array): boolean;"
          , "    close(): void;"
          , "}"
          ]
    let tsIfaceOrderB = T.unlines
          [ "export interface DataService {"
          , "    close(): void;"
          , "    write(key: string, val: Uint8Array): boolean;"
          , "    read(key: string): Uint8Array;"
          , "}"
          ]

    it "produces bit-identical F_T when interface methods are permuted" $ do
      case (parsePolyglotSource "service.ts" tsIfaceOrderA, parsePolyglotSource "service.ts" tsIfaceOrderB) of
        (Right pA, Right pB) -> do
          let ftA = computeFT pA
              ftB = computeFT pB
          ftA `shouldBe` ftB
          unFingerprint ftA `shouldNotBe` ""
        (Left e1, _) -> expectationFailure (show e1)
        (_, Left e2) -> expectationFailure (show e2)

  describe "Structural Subtyping & Equivalence" $ do
    let mRead = MethodContract "read" [TypePrimitive "string"] (TypeArray (TypePrimitive "byte")) False
        mWrite = MethodContract "write" [TypePrimitive "string", TypeArray (TypePrimitive "byte")] (TypePrimitive "bool") False
        mClose = MethodContract "close" [] (TypePrimitive "void") False

    let contractSuper = InterfaceContract "Reader" [mClose, mRead] []
    let contractSub = InterfaceContract "FullService" [mClose, mRead, mWrite] []
    let contractNominalOther = InterfaceContract "IReader" [mClose, mRead] []

    it "identifies structural equality between differently named interfaces" $ do
      areStructurallyEqual contractSuper contractNominalOther `shouldBe` True

    it "validates structural subtyping when sub-contract contains superset of methods" $ do
      isSubtypeOf contractSub contractSuper `shouldBe` True
      isSubtypeOf contractSuper contractSub `shouldBe` False

  describe "Type Contract Sensitivity" $ do
    let tsBase = T.unlines
          [ "export interface TokenService {"
          , "    generate(id: string): string;"
          , "}"
          ]
    let tsMutatedReturn = T.unlines
          [ "export interface TokenService {"
          , "    generate(id: string): number;"
          , "}"
          ]
    let tsMutatedParam = T.unlines
          [ "export interface TokenService {"
          , "    generate(id: string, salt: string): string;"
          , "}"
          ]

    it "sensitively alters F_T when return type changes" $ do
      case (parsePolyglotSource "token.ts" tsBase, parsePolyglotSource "token.ts" tsMutatedReturn) of
        (Right p1, Right p2) -> computeFT p1 `shouldNotBe` computeFT p2
        _ -> expectationFailure "Parse failure in sensitivity test"

    it "sensitively alters F_T when parameter signature changes" $ do
      case (parsePolyglotSource "token.ts" tsBase, parsePolyglotSource "token.ts" tsMutatedParam) of
        (Right p1, Right p2) -> computeFT p1 `shouldNotBe` computeFT p2
        _ -> expectationFailure "Parse failure in sensitivity test"

  describe "Go Structural Interface Normalization" $ do
    let goIfaceA = T.unlines
          [ "package store"
          , ""
          , "type Storage interface {"
          , "    Get(key string) ([]byte, error)"
          , "    Put(key string, val []byte) error"
          , "}"
          ]
    let goIfaceB = T.unlines
          [ "package store"
          , ""
          , "type Storage interface {"
          , "    Put(key string, val []byte) error"
          , "    Get(key string) ([]byte, error)"
          , "}"
          ]

    it "produces identical F_T for permuted Go interfaces" $ do
      case (parsePolyglotSource "store.go" goIfaceA, parsePolyglotSource "store.go" goIfaceB) of
        (Right pA, Right pB) -> computeFT pA `shouldBe` computeFT pB
        (Left e1, _) -> expectationFailure (show e1)
        (_, Left e2) -> expectationFailure (show e2)

  describe "Extended Structural Type Algebraic Properties" $ do
    it "guarantees three-member union commutativity and canonical sorting" $ do
      let u1 = parseTypeString "boolean | number | string"
          u2 = parseTypeString "string | boolean | number"
          u3 = parseTypeString "number | string | boolean"
      u1 `shouldBe` u2
      u2 `shouldBe` u3

    it "guarantees idempotent deduplication in complex unions" $ do
      let u1 = parseTypeString "string | number | string | boolean | number | boolean"
          u2 = parseTypeString "boolean | number | string"
      u1 `shouldBe` u2

    it "guarantees three-member intersection commutativity" $ do
      let i1 = parseTypeString "Alpha & Beta & Gamma"
          i2 = parseTypeString "Gamma & Beta & Alpha"
      i1 `shouldBe` i2

    it "proves subtyping reflexivity (A <= A for all contracts)" $ do
      let m = MethodContract "exec" [TypePrimitive "int"] (TypePrimitive "void") False
          c = InterfaceContract "Runner" [m] []
      isSubtypeOf c c `shouldBe` True

    it "proves subtyping transitivity (A <= B and B <= C implies A <= C)" $ do
      let mA = MethodContract "a" [] (TypePrimitive "void") False
          mB = MethodContract "b" [] (TypePrimitive "void") False
          mC = MethodContract "c" [] (TypePrimitive "void") False
          contractC = InterfaceContract "Base" [mA] []
          contractB = InterfaceContract "Middle" [mA, mB] []
          contractA = InterfaceContract "Top" [mA, mB, mC] []
      isSubtypeOf contractA contractB `shouldBe` True
      isSubtypeOf contractB contractC `shouldBe` True
      isSubtypeOf contractA contractC `shouldBe` True

    it "detects non-subtyping when a required method is absent" $ do
      let mA = MethodContract "read" [] (TypePrimitive "string") False
          mB = MethodContract "write" [TypePrimitive "string"] (TypePrimitive "void") False
          cReader = InterfaceContract "Reader" [mA] []
          cWriter = InterfaceContract "Writer" [mB] []
      isSubtypeOf cReader cWriter `shouldBe` False
      isSubtypeOf cWriter cReader `shouldBe` False

    it "handles empty interface with zero methods" $ do
      let cEmpty = InterfaceContract "Any" [] []
          m = MethodContract "ping" [] (TypePrimitive "bool") False
          cFull = InterfaceContract "Pinger" [m] []
      isSubtypeOf cFull cEmpty `shouldBe` True
      isSubtypeOf cEmpty cFull `shouldBe` False

    it "Rust: produces identical F_T when Trait methods are permuted" $ do
      let rsTraitA = T.unlines
            [ "pub trait Processor {"
            , "    fn process(&self) -> bool;"
            , "    fn reset(&mut self);"
            , "}"
            ]
      let rsTraitB = T.unlines
            [ "pub trait Processor {"
            , "    fn reset(&mut self);"
            , "    fn process(&self) -> bool;"
            , "}"
            ]
      case (parsePolyglotSource "proc.rs" rsTraitA, parsePolyglotSource "proc.rs" rsTraitB) of
        (Right pA, Right pB) -> computeFT pA `shouldBe` computeFT pB
        (Left e1, _) -> expectationFailure (show e1)
        (_, Left e2) -> expectationFailure (show e2)

    it "sensitively alters F_T when method count changes in interface" $ do
      let ts1 = "export interface Svc { run(): void; }"
          ts2 = "export interface Svc { run(): void; stop(): void; }"
      case (parsePolyglotSource "s1.ts" ts1, parsePolyglotSource "s2.ts" ts2) of
        (Right p1, Right p2) -> computeFT p1 `shouldNotBe` computeFT p2
        _ -> expectationFailure "Parse failed"

    it "sensitively alters F_T when method name changes in interface" $ do
      let ts1 = "export interface Calc { add(x: number): number; }"
          ts2 = "export interface Calc { sum(x: number): number; }"
      case (parsePolyglotSource "c1.ts" ts1, parsePolyglotSource "c2.ts" ts2) of
        (Right p1, Right p2) -> computeFT p1 `shouldNotBe` computeFT p2
        _ -> expectationFailure "Parse failed"

    it "sensitively alters F_T when method parameter type changes" $ do
      let ts1 = "export interface Validator { check(val: string): boolean; }"
          ts2 = "export interface Validator { check(val: number): boolean; }"
      case (parsePolyglotSource "v1.ts" ts1, parsePolyglotSource "v2.ts" ts2) of
        (Right p1, Right p2) -> computeFT p1 `shouldNotBe` computeFT p2
        _ -> expectationFailure "Parse failed"

    it "produces non-empty deterministic F_T hash for polyglot interfaces" $ do
      let ts = "export interface Api { fetch(url: string): string; }"
      case parsePolyglotSource "api.ts" ts of
        Right p -> unFingerprint (computeFT p) `shouldNotBe` ""
        Left err -> expectationFailure (show err)

  describe "Phase 1: Core 9-Tier Identity Matrix & Type Contract Promotion" $ do

    describe "Full 9-Tier Bundle Construction" $ do
      it "populates fTTypeContract in FingerprintBundle from computeBundle" $ do
        let tsCode = "export interface Greeter { greet(name: string): string; }"
        case computeBundleFromSource "greeter.ts" tsCode of
          Left err -> expectationFailure (show err)
          Right b -> do
            unFingerprint (fTTypeContract b) `shouldNotBe` ""
            unFingerprint (f4Composite b) `shouldNotBe` ""

      it "populates fTTypeContract in computeProgramFingerprints" $ do
        let goCode = "package svc\ntype Storage interface {\n    Save(data []byte) error\n}\n"
        case parsePolyglotSource "storage.go" goCode of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let b = computeProgramFingerprints prog
            unFingerprint (fTTypeContract b) `shouldNotBe` ""
            unFingerprint (f4Composite b) `shouldNotBe` ""

    describe "Composite F4 9-Tier Invariance & Sensitivity" $ do
      it "guarantees F_T invariance under interface method permutation while F4 reflects AST order" $ do
        let tsA = "export interface Service { a(): void; b(): number; }"
            tsB = "export interface Service { b(): number; a(): void; }"
        case (computeBundleFromSource "s.ts" tsA, computeBundleFromSource "s.ts" tsB) of
          (Right bA, Right bB) -> do
            fTTypeContract bA `shouldBe` fTTypeContract bB
            unFingerprint (fTTypeContract bA) `shouldNotBe` ""
          _ -> expectationFailure "Bundle computation failed"

      it "guarantees full F1..F4 and FT invariance under pure top-level interface permutation" $ do
        let tsA = "export interface ServiceA { run(): void; }\nexport interface ServiceB { stop(): void; }\n"
            tsB = "export interface ServiceB { stop(): void; }\nexport interface ServiceA { run(): void; }\n"
        case (computeBundleFromSource "s.ts" tsA, computeBundleFromSource "s.ts" tsB) of
          (Right bA, Right bB) -> do
            f1Structural bA `shouldBe` f1Structural bB
            f2Declaration bA `shouldBe` f2Declaration bB
            fTTypeContract bA `shouldBe` fTTypeContract bB
            f4Composite bA `shouldBe` f4Composite bB
          _ -> expectationFailure "Bundle computation failed"

      it "sensitively alters F4 composite when type contract method signature changes" $ do
        let tsA = "export interface Repo { find(id: string): string; }"
            tsB = "export interface Repo { find(id: number): string; }"
        case (computeBundleFromSource "r.ts" tsA, computeBundleFromSource "r.ts" tsB) of
          (Right bA, Right bB) -> do
            fTTypeContract bA `shouldNotBe` fTTypeContract bB
            f4Composite bA `shouldNotBe` f4Composite bB
          _ -> expectationFailure "Bundle computation failed"

    describe "9-Tier Semantic Invariant Comparison (crTypeContract)" $ do
      it "evaluates crTypeContract as Identical for equivalent type contracts" $ do
        let go1 = "package api\ntype Writer interface { Write(p []byte) (n int, err error) }\n"
            go2 = "package api\ntype Writer interface {\n    Write(p []byte) (n int, err error)\n}\n"
        case (computeBundleFromSource "w1.go" go1, computeBundleFromSource "w2.go" go2) of
          (Right b1, Right b2) -> do
            let cr = compareBundles b1 b2
            crTypeContract cr `shouldBe` Identical
            crComposite cr `shouldBe` Identical
          _ -> expectationFailure "Bundle computation failed"

      it "evaluates crTypeContract as Different when interface method return type changes" $ do
        let ts1 = "export interface Worker { doWork(): boolean; }"
            ts2 = "export interface Worker { doWork(): number; }"
        case (computeBundleFromSource "w1.ts" ts1, computeBundleFromSource "w2.ts" ts2) of
          (Right b1, Right b2) -> do
            let cr = compareFingerprints b1 b2
            crTypeContract cr `shouldBe` Different
            crComposite cr `shouldBe` Different
          _ -> expectationFailure "Bundle computation failed"

      it "formats comparison result including FT (Type Contract) tier" $ do
        let b1 = FingerprintBundle (Fingerprint "s") (Fingerprint "st") (Fingerprint "dc") (Fingerprint "dp") (Fingerprint "cg") (Fingerprint "cf") (Fingerprint "df") (Fingerprint "tc") (Fingerprint "cp")
            b2 = FingerprintBundle (Fingerprint "s") (Fingerprint "st") (Fingerprint "dc") (Fingerprint "dp") (Fingerprint "cg") (Fingerprint "cf") (Fingerprint "df") (Fingerprint "diff_tc") (Fingerprint "diff_cp")
            cr = compareBundles b1 b2
            fmt = formatComparisonResult cr
        T.isInfixOf "FT  (Type Contract):" fmt `shouldBe` True
        T.isInfixOf "DIFFERENT" fmt `shouldBe` True

    describe "Backward-Compatible JSON Serialization & Parsing" $ do
      it "roundtrips FingerprintBundle with type_contract to and from JSON" $ do
        let b = FingerprintBundle (Fingerprint "s") (Fingerprint "st") (Fingerprint "dc") (Fingerprint "dp") (Fingerprint "cg") (Fingerprint "cf") (Fingerprint "df") (Fingerprint "tc123") (Fingerprint "cp")
            encoded = Aeson.encode b
        Aeson.decode encoded `shouldBe` Just b

      it "deserializes historical manifests missing type_contract cleanly defaulting to empty Fingerprint" $ do
        let legacyJson = "{\"source\":\"s\",\"structural\":\"st\",\"declaration\":\"dc\",\"dependency\":\"dp\",\"call_graph\":\"cg\",\"control_flow\":\"cf\",\"data_flow\":\"df\",\"composite\":\"cp\"}" :: BL.ByteString
        case Aeson.decode legacyJson of
          Nothing -> expectationFailure "Failed to parse legacy FingerprintBundle JSON"
          Just b -> do
            f0Source b `shouldBe` Fingerprint "s"
            f1Structural b `shouldBe` Fingerprint "st"
            fTTypeContract b `shouldBe` Fingerprint ""
            f4Composite b `shouldBe` Fingerprint "cp"

      it "roundtrips ComparisonResult with type_contract to and from JSON" $ do
        let cr = ComparisonResult Identical Identical Identical Identical Identical Identical Identical Different Different
            encoded = Aeson.encode cr
        Aeson.decode encoded `shouldBe` Just cr

      it "deserializes historical ComparisonResult missing type_contract defaulting to Identical" $ do
        let legacyCrJson = "{\"source\":\"identical\",\"structural\":\"identical\",\"declaration\":\"identical\",\"dependency\":\"identical\",\"call_graph\":\"identical\",\"control_flow\":\"identical\",\"data_flow\":\"identical\",\"composite\":\"identical\"}" :: BL.ByteString
        case Aeson.decode legacyCrJson of
          Nothing -> expectationFailure "Failed to parse legacy ComparisonResult JSON"
          Just cr -> do
            crStructural cr `shouldBe` Identical
            crTypeContract cr `shouldBe` Identical
            crComposite cr `shouldBe` Identical

    describe "Change Impact Analysis Classification for Type Contracts" $ do
      it "classifies type contract mutations as SeverityInterface" $ do
        let bOld = FingerprintBundle (Fingerprint "s") (Fingerprint "st") (Fingerprint "dc") (Fingerprint "dp") (Fingerprint "cg") (Fingerprint "cf") (Fingerprint "df") (Fingerprint "tc_old") (Fingerprint "c1")
            bNew = FingerprintBundle (Fingerprint "s") (Fingerprint "st") (Fingerprint "dc") (Fingerprint "dp") (Fingerprint "cg") (Fingerprint "cf") (Fingerprint "df") (Fingerprint "tc_new") (Fingerprint "c2")
        classifySeverity bOld bNew `shouldBe` SeverityInterface
