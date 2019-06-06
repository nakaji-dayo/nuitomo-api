module DBError (
  module DBError,
  SqlError(..)
) where

import           Control.Monad.Catch
import           Database.HDBC           (handleSql)
import           Database.HDBC.Statement

data DBError =
  ForeignKeyViolation
  | UniqueViolation
  | OtherDBError SqlError deriving (Eq, Show)

type ParseSqlError = SqlError -> DBError

newtype SqlException = SqlException SqlError deriving (Show, Eq)
instance Exception SqlException

exceptSql :: IO a -> IO a
exceptSql = handleSql (throwM . SqlException)