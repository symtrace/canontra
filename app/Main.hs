{- |
Module      : Main
Description : Executable entry point for canontra CLI.

This is the top-level executable harness. It hands off execution
immediately to the CLI dispatcher.
-}
module Main (main) where

import Canontra.CLI.Commands (runCLI)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO (hSetEncoding, stderr, stdout)

main :: IO () -- e.g. entrypoint for canontra binary
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  runCLI
