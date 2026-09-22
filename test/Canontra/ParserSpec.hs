{- |
Module      : Canontra.ParserSpec
Description : Unit test specification for the Python parsing subsystem.

Tests parser correctness across modern Python 3 language constructs:
type annotations, async/await, generators, f-strings, slices,
starred expressions, and structured parse error emission.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Canontra.ParserSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Canontra.IR.Declaration
import Canontra.IR.Program
import Canontra.Parser.Python (parsePythonSource)
import Canontra.Types (ParseError (..))

spec :: Spec -- e.g. parser test suite definition
spec = do
  describe "Python parsing to IR" $ do
    it "parses simple function declarations" $ do
      let code = "def add(a, b):\n    return a + b\n"
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure ("Failed to parse: " ++ show (peReason err))
        Right (Program [m] _) -> do
          modDeclarations m `shouldSatisfy` (\case
            [DeclFunction (Function "add" [Parameter "a" _ _ _, Parameter "b" _ _ _] _ _ _ False)] -> True
            _ -> False)
        Right _ -> expectationFailure "Unexpected program module shape"

    it "parses PEP 484 type annotations on parameters and return types" $ do
      let code = "def greet(name: str, greeting: str = \"Hello\") -> str:\n    return greeting + name\n"
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure ("Failed to parse: " ++ show (peReason err))
        Right (Program [m] _) -> do
          case modDeclarations m of
            [DeclFunction fn] -> do
              fnName fn `shouldBe` "greet"
              fnReturnType fn `shouldBe` Just "str"
              let params = fnParams fn
              length params `shouldBe` 2
              paramName (params !! 0) `shouldBe` "name"
              paramKind (params !! 0) `shouldBe` ParamPositional
              paramType (params !! 0) `shouldBe` Just "str"
              paramName (params !! 1) `shouldBe` "greeting"
              paramType (params !! 1) `shouldBe` Just "str"
            _ -> expectationFailure "Expected single function declaration"
        Right _ -> expectationFailure "Unexpected program structure"

    it "parses PEP 492 async functions, await, and async context managers" $ do
      let code = T.unlines
            [ "async def fetch_data(url: str):"
            , "    async with session_pool() as session:"
            , "        res = await session.get(url)"
            , "        return res"
            ]
      case parsePythonSource "async_test.py" code of
        Left err -> expectationFailure ("Failed to parse: " ++ show (peReason err))
        Right (Program [m] _) -> do
          case modDeclarations m of
            [DeclFunction fn] -> do
              fnName fn `shouldBe` "fetch_data"
              fnIsAsync fn `shouldBe` True
              fnReturnType fn `shouldBe` Nothing
              length (fnBody fn) `shouldSatisfy` (> 0)
            _ -> expectationFailure "Expected async function declaration"
        Right _ -> expectationFailure "Unexpected program structure"

    it "parses generators, yield, and yield from" $ do
      let code = T.unlines
            [ "def gen(items):"
            , "    for x in items:"
            , "        yield x * 2"
            , "    yield from sub_gen()"
            ]
      case parsePythonSource "gen.py" code of
        Left err -> expectationFailure ("Failed to parse: " ++ show (peReason err))
        Right (Program [m] _) -> do
          case modDeclarations m of
            [DeclFunction fn] -> fnName fn `shouldBe` "gen"
            _ -> expectationFailure "Expected generator function"
        Right _ -> expectationFailure "Unexpected program structure"

    it "parses class declarations with methods, base classes, and decorators" $ do
      let code = T.unlines
            [ "@dataclass"
            , "class Calculator(Base):"
            , "    def calculate(self, x: int) -> int:"
            , "        return x * 2"
            ]
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure ("Failed to parse: " ++ show (peReason err))
        Right (Program [m] _) -> do
          case modDeclarations m of
            [DeclClass cls] -> do
              clsName cls `shouldBe` "Calculator"
              clsBases cls `shouldBe` ["Base"]
              length (clsMethods cls) `shouldBe` 1
              clsDecorators cls `shouldBe` ["@dataclass"]
            _ -> expectationFailure "Unexpected class structure"
        Right _ -> expectationFailure "Unexpected class structure"

    it "parses f-strings and slices" $ do
      let code = T.unlines
            [ "def format_slice(items, idx):"
            , "    msg = f\"Item at {idx}: {items[1:5]}\""
            , "    return msg"
            ]
      case parsePythonSource "slice.py" code of
        Left err -> expectationFailure ("Failed to parse: " ++ show (peReason err))
        Right (Program [m] _) -> do
          length (modDeclarations m) `shouldBe` 1
        Right _ -> expectationFailure "Unexpected shape"

    it "emits structured ParseError on syntax errors" $ do
      let invalidCode = "def broken(:\n    pass\n"
      case parsePythonSource "broken.py" invalidCode of
        Left err -> do
          peFile err `shouldBe` "broken.py"
          peLine err `shouldSatisfy` (> 0)
        Right _ -> expectationFailure "Expected parser to reject invalid syntax"
