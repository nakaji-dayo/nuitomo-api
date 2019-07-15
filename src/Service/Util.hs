module Service.Util where

import           App
import           Data.Time.Clock       (UTCTime, diffUTCTime)
import           Data.Time.Clock.POSIX as C
import           Data.Time.LocalTime

getCurrentLocalTime :: MonadService m => m LocalTime
getCurrentLocalTime = utcToLocalTime utc <$> App.getCurrentTime
