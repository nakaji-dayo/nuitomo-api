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
    "gen-key"   -> printNewKey
    "gen-token" -> do
      let uid:_ = opts
      genToken uid
    "hello"     -> runBatchM Batch.Hello.execute
    _           -> putStrLn "unknown command"

genToken :: String -> IO ()
genToken uid = do
  config <- Config.loadConfig
  let jwtCfg = mkJwtCfg (Config.apiSecretKey config)
  print =<< generateToken jwtCfg (read uid)