{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module Service.User where

import           App
import           Auth
import           Data.Maybe
import           Data.Type.Map     as TM
import           Entity
import           Entity.Follow
import           Entity.OwnerUser
import           Entity.User       as E
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
        , bio = ""
        , nickname = ""
        , gender = ""
        , hometown = ""
        , entryDate = ""
        , favoriteThing = ""
        , dislikeThing = ""
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

updateUser :: MonadService m => AccountId -> ResourceId -> UpdateUserRequest -> m ()
updateUser (AccountId aid) uid req = do
  u <- getOwnUser aid uid
  let u' = u
        { name = fromMaybe (u ^. #name) (req ^. #name)
        , bio = fromMaybe (u ^. #bio) (req ^. #bio)
        , nickname = fromMaybe (u ^. #nickname) (req ^. #nickname)
        , gender = fromMaybe (u ^. #gender) (req ^. #gender)
        , hometown = fromMaybe (u ^. #hometown) (req ^. #hometown)
        , entryDate = fromMaybe (u ^. #entryDate) (req ^. #entryDate)
        , favoriteThing = fromMaybe (u ^. #favoriteThing) (req ^. #favoriteThing)
        , dislikeThing = fromMaybe (u ^. #dislikeThing) (req ^. #dislikeThing)
        } :: User
  keyUpdateM E.updateUser u'
  pure ()


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
