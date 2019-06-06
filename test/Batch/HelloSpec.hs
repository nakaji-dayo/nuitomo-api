{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
module Batch.HelloSpec where

import           Test.Hspec

spec :: Spec
spec =
  describe "execute" $
    it "1" $
      (1 :: Int) `shouldBe` 1