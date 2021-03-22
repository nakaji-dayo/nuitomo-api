{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Service.Util where

import           App
import           Data.Time.LocalTime
import           Service.Exception

getCurrentLocalTime :: MonadService m => m LocalTime
getCurrentLocalTime = utcToLocalTime utc <$> App.getCurrentTime


getResource :: (MonadService m, ToSql SqlValue p, FromSql SqlValue b, Show p, Eq p) => Query p b -> p -> m b
getResource q p = queryM q p >>= \case
  x:_ -> pure x
  _ -> throwM ResourceNotExist
