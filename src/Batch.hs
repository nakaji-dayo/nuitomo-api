{-# LANGUAGE OverloadedStrings #-}
module Batch where

import           App
import           Control.Monad.Except (runExceptT)
import           DataSource

runBatchM :: AppM a -> IO ()
runBatchM f = do
  (ctx, logEnv) <- initialize
  _ <- runExceptT $ runHandler' $ nt ctx (logEnv, "runBatchM") f
  destroyAllResources $ pool ctx
  return ()