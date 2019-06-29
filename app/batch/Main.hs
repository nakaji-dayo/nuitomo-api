module Main where

import           Auth
import           Batch
import           Batch.Hello
import qualified Config
import           System.Environment

main :: IO ()
main = do
  cmd:opts <- getArgs
  case cmd of
    "hello" -> runBatchM Batch.Hello.execute
    _       -> putStrLn "unknown command"
