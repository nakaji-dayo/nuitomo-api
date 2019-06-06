{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instance where

import qualified Data.Aeson          as A
import           Data.Default.Class
import           Data.Proxy
import           Data.Scientific
import           Data.String
import           Data.Swagger
import qualified Data.Text           as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Test.QuickCheck

instance Arbitrary LocalTime where
   arbitrary = do
     d <- ModifiedJulianDay <$> arbitrary
     t <- TimeOfDay <$> arbitrary <*> arbitrary <*> arbitrary
     return $ LocalTime d t

instance Default LocalTime where
  def = LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 0 0)

instance Default Scientific where
  def = 0

instance Arbitrary UTCTime where
   arbitrary =
     localTimeToUTC utc <$> arbitrary

instance Default UTCTime where
  def = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

instance Arbitrary Day where
   arbitrary = ModifiedJulianDay <$> arbitrary

instance Default Day where
  def = ModifiedJulianDay 0

instance IsString Day where
  fromString = read

instance Default Bool where
  def = False

instance Default A.Value where
  def = A.Null

instance Arbitrary A.Value where
  arbitrary = A.String . T.pack <$> arbitrary

-- 実際にはAnyValue
instance ToSchema A.Value where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary