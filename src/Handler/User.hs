{-# LANGUAGE OverloadedLabels #-}
module Handler.User where

import           App
import           Auth
import           Data.Maybe
import           Data.Type.Map        as TM
import           EntityId
import           Handler.Middleware
import           Service.Notification
import           Service.Owner
import           Service.Post
import           Service.User
import           Type                 as T
import           Util
import           View

loadRenderUsers xs =  do
  loaded <- snd <$> loadUserRelation xs TM.Empty
  runViewM $ mapM (renderUser loaded) xs

getUsersR :: AccountId -> AppM [UserResponse]
getUsersR i = getUsers i >>= loadRenderUsers

getUsersSearchR :: AccountId -> Maybe String -> AppM [UserResponse]
getUsersSearchR _ q = searchUser q >>= loadRenderUsers

getUserR :: AccountId -> UserId -> AppM DetailUserResponse
getUserR (AccountId aid) uid = do
  x <- getUser aid uid
  loaded <- snd <$> loadUserRelation [fst x] TM.Empty
  runViewM $ renderDetailUser loaded x

postUsersR :: AccountId -> CreateUserRequest ->  AppM UserId
postUsersR i req =
  createUser i (req ^. #name) (req ^. #image)

postFollowsR :: AccountId -> UserId -> CreateFollowRequest -> AppM ()
postFollowsR a u r = createFollow (unAccountId a) u (r ^. #toUserId)

deleteFollowsR :: AccountId -> UserId -> CreateFollowRequest -> AppM ()
deleteFollowsR a u r = deleteFollow (unAccountId a) u (r ^. #toUserId)

getFolloweesR :: AccountId -> UserId -> AppM [UserResponse]
getFolloweesR _ uid = getFollowees uid >>= loadRenderUsers

getFollowersR :: AccountId -> UserId -> Maybe String -> AppM [UserResponse]
getFollowersR aid uid (Just "home") = getFollowers (Just aid) uid >>= loadRenderUsers
getFollowersR _ uid _        = getFollowers Nothing uid >>= loadRenderUsers

patchUserR :: AccountId -> UserId -> UpdateUserRequest -> AppM ()
patchUserR = updateUser

getNotificationsR :: AccountId -> Maybe NotificationId -> AppM [GetNotification]
getNotificationsR a mcursor = do
  ns <- getNotifications a mcursor
  c <- snd <$> loadNotificationRelation a ns TM.Empty
  runViewM $ mapM (renderNotification c) ns

getMeR :: AccountId -> AppM MeResponse
getMeR a =
  MeResponse <$> getOwnerKey (unAccountId a)

getOwnersR :: AccountId -> UserId -> AppM [OwnerResponse]
getOwnersR (AccountId a) uid = do
  xs <- getOwners a uid
  runViewM $ mapM renderOwnerResponse xs

postOwnersR :: AccountId -> UserId -> String -> AppM ()
postOwnersR (AccountId a) u k = do
  liftIO $ putStrLn "test"
  addOwner a u k

deleteOwnersR :: AccountId -> OwnerUserId -> AppM ()
deleteOwnersR (AccountId a) = deleteOwner a
