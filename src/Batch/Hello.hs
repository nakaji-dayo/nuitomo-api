module Batch.Hello where

import           App

execute :: AppM ()
execute = liftIO $ putStrLn "Hello, Batch"