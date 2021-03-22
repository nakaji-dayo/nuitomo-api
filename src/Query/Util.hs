{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Query.Util where

import           Database.Record                        (PersistableWidth)
import           Database.Relational
import           Database.Relational.Projectable.Unsafe (OperatorContext)
import qualified Language.SQL.Keyword.Type              as K

values' ::
  (LiteralSQL t,
   OperatorContext c, Num t) =>
  [t] -> RecordList (Record c) t
values' [] = values [-1]
values' x  = values x

values'' :: (Num a, PersistableWidth a, LiteralSQL a, OperatorContext c0) => [Maybe a] -> RecordList (Record c0) (Maybe a)
values'' [] = values [Just (-1)]
values'' xs = values xs

limit' :: Int -> QuerySuffix
limit' n = ["limit", K.word (show n)]
