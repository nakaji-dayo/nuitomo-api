{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}
module Batch.Question where

import           App
import           Auth
import           Batch
import           Control.Lens
import           Control.Monad
import           Entity
import           Logger
import           Query
import           Service.Post
import           Service.User
import           System.Random

execute :: AppM ()
execute = do
  $logInfoM "Batch.Question"
  us <- queryM selectAllUser ()
  forM_ us $ \u -> do
    r <- liftIO $ randomRIO (0 :: Int, 100)
    when (r < 10 && u ^. #id /= robotUserId) $ do
      createQuestionPost (u ^. #id)
  pure ()
