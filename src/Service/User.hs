{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module Service.User where

import           App
import           Auth
import           Data.Type.Map     as TM
import           Entity
import           Entity.Follow
import           Entity.OwnerUser
import           Entity.User
import           Entity.UserImage
import           Query             as Q
import           Service.Exception
import           Service.Loader
import           Type

getResource q p = queryM q p >>= \case
  x:_ -> pure x
  _ -> throwM ResourceNotExist

getUser :: MonadService m => ResourceId -> m User
getUser = getResource selectUser

getUsers :: MonadService m => AccountId -> m [User]
getUsers aid = queryM selectUsers (unAccountId aid)

searchUser :: MonadService m => Maybe String -> m [User]
searchUser x = queryM  (selectUserSearch x) ()

createUser :: MonadService m => AccountId -> String -> String -> m ResourceId
createUser (AccountId aid) name imageUrl = do
  uid <- getTid
  uiid <- getTid
  oid <- getTid
  let ui = UserImage
        { id = uiid
        , userId = uid
        , url = imageUrl
        }
      u = User
        { id = uid
        , name = name
        }
      ou = OwnerUser
        { id = oid
        , ownerId = aid
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

getOwnUser :: MonadService m => String -> ResourceId -> m User
getOwnUser a u = getResource selectOwnUser (a, u)

createFollow a uid toUid = do
  getOwnUser a uid
  tid <- getTid
  let f = Follow
        { id = tid
        , userId = uid
        , toUserId = toUid
        }
  insertM insertFollow f
  pure ()

deleteFollow a uid toUid = do
  getOwnUser a uid
  deleteM Q.deleteFollow (uid, toUid)
  pure ()


getFollowees :: MonadService m => ResourceId -> m [User]
getFollowees uid = queryM selectFollowees uid

getFollowers :: MonadService m => ResourceId -> m [User]
getFollowers uid = queryM selectFollowers uid
