module Main where

import qualified Spec
import           Test.DocTest
import           Test.Hspec.Formatters
import           Test.Hspec.Runner

main :: IO ()
main = do
  hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
  doctest ["-isrc", "src/Util.hs"]