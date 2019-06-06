-- todo: 再設計
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.Exception
  ( module Service.Exception
  , throwM
  ) where

import           App
import           Control.Lens
import           Control.Monad.Catch  (Exception, throwM)
import           Data.ByteString.Lazy
import           Data.Typeable
import           DBError
import           DBError.Postgres

data AuthFailedDetail =
  InvalidAccessToken
  | InvalidFacebookUserInfo ByteString
  | FacebookApiError
  | InvalidLinkbalIdUserInfo ByteString
  | LinkbalIdApiError
  | SigninAsBannedUser
  | UserNotOldEnough
  deriving (Show, Typeable)

data ServiceException =
  ResourceNotExist
  | Unauthorized
  | ResourceConflict
  | OtherException String
  deriving (Show, Typeable)
instance Exception ServiceException


generalDBErrorHandler :: MonadService m => SqlError -> m x
generalDBErrorHandler = h . parseSqlError
  where
    h ForeignKeyViolation = throwM ResourceNotExist
    h UniqueViolation     = throwM ResourceConflict
    h (OtherDBError e)    = throwM $ OtherException (show e)

makePrisms ''ServiceException