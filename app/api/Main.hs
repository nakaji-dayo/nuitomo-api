{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Api
import           Batch
import           Batch.Question
import           Control.Concurrent
import           System.Cron

main :: IO ()
main = do
  forkIO cronJob
  serve

cronJob :: IO ()
cronJob = do
  tids <- execSchedule $ do
    addJob (runBatchM Batch.Question.execute) "0 12 * * *"
  print tids
