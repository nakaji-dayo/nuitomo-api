{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Service.Util where

import           App
import           Data.Time.Clock       (UTCTime, diffUTCTime)
import           Data.Time.Clock.POSIX as C
import           Data.Time.LocalTime
import           Service.Exception

getCurrentLocalTime :: MonadService m => m LocalTime
getCurrentLocalTime = utcToLocalTime utc <$> App.getCurrentTime


getResource q p = queryM q p >>= \case
  x:_ -> pure x
  _ -> throwM ResourceNotExist
