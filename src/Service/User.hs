{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module Service.User where

import           App
import           Auth
import           Control.Monad
import           Data.Maybe
import           Data.Type.Map        as TM
import           EagerLoader
import           Entity
import           Entity.Follow
import           Entity.OwnerUser
import           Entity.User          as E
import           Entity.UserImage
import           Query                as Q
import           Service.Loader
import           Service.Notification
import           Service.Util
import           Type
import           Util

getUser :: MonadService m => String -> ResourceId -> m (User, Maybe OwnerUser)
getUser = curry (getResource Q.selectUser)

getUsers :: MonadService m => AccountId -> m [User]
getUsers aid = queryM selectUsers (unAccountId aid)

searchUser :: MonadService m => Maybe String -> m [User]
searchUser x = queryM  (selectUserSearch x) ()

createUser :: MonadService m => AccountId -> String -> String -> m ResourceId
createUser (AccountId aid) name imagePath = do
  uid <- getTid
  uiid <- getTid
  oid <- getTid
  now <- getCurrentLocalTime
  let ui = UserImage
        { id = uiid
        , userId = uid
        , path = imagePath
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
        , createdAt = now
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
  runTransactionM $ do
    forM_ (req ^. #image) $ \imagePath -> do
      ui <- getResource selectUserImages uid
      let ui' = ui {path = imagePath}
      void $ keyUpdateM updateUserImage ui'
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
  runTransactionM $ do
    insertM insertFollow f
    createNotification  toUid NotifyFollow (Just uid) Nothing
  pure ()

deleteFollow a uid toUid = do
  getOwnUser a uid
  deleteM Q.deleteFollow (uid, toUid)
  pure ()


getFollowees :: MonadService m => ResourceId -> m [User]
getFollowees uid = queryM selectFollowees uid

getFollowers :: MonadService m => Maybe AccountId -> ResourceId -> m [User]
getFollowers maid uid = queryM (selectFollowers (unAccountId <$> maid)) uid

getNotifications :: MonadService m => AccountId -> m [(Notification, User, Maybe User, Maybe Post)]
getNotifications (AccountId uid) = queryM selectNotifications uid

loadNotificationRelation (AccountId aid) xs ctx =
  loadPostImages (Var :: Var "postImages") (mapMaybe (^. #refPostId) $ fmap fst4 xs) ctx
  ->> loadPostReplies (Var :: Var "replies") [] -- TODO: ダミー。クエリ発行しないようにすべき
  ->> loadPostLikes aid (Var :: Var "postLikes") [] -- TODO: ダミー。クエリ発行しないようにすべき
  ->> loadUser (Var :: Var "users") (((^. #userId) <$> mapMaybe fth4 xs) ++ (mapMaybe (^. #refUserId) (fmap fst4 xs)))
  ->>= (\us -> loadUserRelation (fmap snd4 xs ++ us))
