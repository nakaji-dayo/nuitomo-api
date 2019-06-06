module DBError.Postgres where

import           Database.HDBC.PostgreSQL
import           Database.HDBC.Statement

import           DBError

parseSqlError :: ParseSqlError
parseSqlError e@(SqlError state _ _)
  | state == foreignKeyViolation = ForeignKeyViolation
  | state == uniqueViolation = UniqueViolation
  | otherwise = OtherDBError e