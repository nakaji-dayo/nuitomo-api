{-# LANGUAGE OverloadedStrings #-}

module Query.Util where

import           Database.Relational
import           Database.Relational.Projectable.Unsafe (OperatorContext)
import qualified Language.SQL.Keyword.Type              as K

values' ::
  (LiteralSQL t,
   OperatorContext c, Num t) =>
  [t] -> RecordList (Record c) t
values' [] = values [-1]
values' x  = values x

limit' :: Int -> QuerySuffix
limit' n = ["limit", K.word (show n)]
