module Query.Util where

import           Database.Relational
import           Database.Relational.Projectable.Unsafe (OperatorContext)

values' ::
  (LiteralSQL t,
   OperatorContext c, Num t) =>
  [t] -> RecordList (Record c) t
values' [] = values [-1]
values' x  = values x
