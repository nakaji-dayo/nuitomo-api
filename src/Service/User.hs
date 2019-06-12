{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module Service.User where

import           App
import           Auth
import           Data.Type.Map    as TM
import           Entity
import           Entity.OwnerUser
import           Entity.User
import           Entity.UserImage
import           Query            as Q
import           Service.Loader
import           Type

getUser :: MonadService m => AuthUser -> m [User]
getUser au = queryM selectUsers (au ^. #sub)

createUser :: MonadService m => AuthUser -> String -> String -> m ResourceId
createUser au name imagePath = do
  uid <- getTid
  uiid <- getTid
  oid <- getTid
  let ui = UserImage
        { id = uiid
        , userId = uid
        , path = imagePath
        }
      u = User
        { id = uid
        , name = name
        }
      ou = OwnerUser
        { id = oid
        , ownerId = au ^. #sub
        , userId = uid
        , isPrimary = True
        }
  runTransactionM $ do
    insertM insertUser u
    insertM insertUserImage ui
    insertM insertOwnerUser ou
  return uid


loadUserRelation rs =
  loadUserImages (Var :: Var "userImages") ((^. #id) <$> rs)
