-- todo: ミドルウェア化?
{-# LANGUAGE DuplicateRecordFields #-}

module Handler.Middleware where

import           App
import           Auth        (AuthUser (..))
import           Entity.User
import           Query

authUserM :: AuthUser -> AppM User
authUserM au = do
  let auid = _id au
  muser <- selectOneM selectUser' auid
  maybe (throwError err401) return muser