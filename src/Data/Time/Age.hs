{-# LANGUAGE ViewPatterns #-}

module Data.Time.Age where

import           Data.Time.Calendar
import           Data.Time.Clock

getAge :: Day -> IO Int
getAge birthday = do
  now <- getCurrentTime
  return $  ageAt now birthday

ageAt :: UTCTime -> Day -> Int
ageAt (toGregorian . utctDay -> (y, m, d)) (toGregorian -> (birthY, birthM, birthD))
   =  fromIntegral
      $ y - birthY
      - (if m > birthM || (m == birthM && d >= birthD) then 0 else 1)