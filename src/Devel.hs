{-# LANGUAGE OverloadedStrings #-}
module Devel where

import           App
import           Control.Monad.Except (runExceptT)
import           DataSource
import           Katip
import           Text.Pretty.Simple   (pPrint)

runAppM' :: AppM a -> IO (Either ServantErr a)
runAppM' h = do
  (ctx, logEnv) <- initialize
  r <- runExceptT $ runHandler' $ nt ctx (logEnv, "RunAppM") h
  destroyAllResources (pool ctx)
  return r

runAppM'' :: AppM a -> IO (Either ServantErr a)
runAppM'' = runAppM' . katipNoLogging

pp :: Show a => AppM a -> IO ()
pp a         = do
  r <- runAppM'' a
  case r of
    Right x -> pPrint x
    x       -> pPrint x