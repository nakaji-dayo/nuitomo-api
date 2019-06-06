{-# LANGUAGE TemplateHaskell #-}
module Service.Git where

import           GitHash

getVersion :: String
getVersion =
  let gi = $$tGitInfoCwd
  in giHash gi