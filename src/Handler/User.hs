{-# LANGUAGE OverloadedLabels #-}
module Handler.User where

import           App
import           Auth
import           Data.Maybe
import           Data.Type.Map      as TM
import           Handler.Middleware
import           Service.Owner
import           Service.Post
import           Service.User
import           Type               as T
import           Util
import           View

loadRenderUsers xs =  do
  loaded <- snd <$> loadUserRelation xs TM.Empty
  runViewM $ mapM (renderUser loaded) xs

getUsersR :: AccountId -> AppM [UserResponse]
getUsersR i = getUsers i >>= loadRenderUsers

getUsersSearchR :: AccountId -> Maybe String -> AppM [UserResponse]
getUsersSearchR _ q = searchUser q >>= loadRenderUsers

getUserR :: AccountId -> ResourceId -> AppM DetailUserResponse
getUserR (AccountId aid) uid = do
  x <- getUser aid uid
  loaded <- snd <$> loadUserRelation [fst x] TM.Empty
  runViewM $ renderDetailUser loaded x

postUsersR :: AccountId -> CreateUserRequest ->  AppM ResourceId
postUsersR i req =
  createUser i (req ^. #name) (req ^. #image)

postFollowsR :: AccountId -> ResourceId -> CreateFollowRequest -> AppM ()
postFollowsR a u r = createFollow (unAccountId a) u (r ^. #toUserId)

deleteFollowsR :: AccountId -> ResourceId -> CreateFollowRequest -> AppM ()
deleteFollowsR a u r = deleteFollow (unAccountId a) u (r ^. #toUserId)

getFolloweesR :: AccountId -> ResourceId -> AppM [UserResponse]
getFolloweesR _ uid = getFollowees uid >>= loadRenderUsers

getFollowersR :: AccountId -> ResourceId -> AppM [UserResponse]
getFollowersR _ uid = getFollowers uid >>= loadRenderUsers

patchUserR :: AccountId -> ResourceId -> UpdateUserRequest -> AppM ()
patchUserR = updateUser

getNotificationsR :: AccountId -> AppM [GetNotification]
getNotificationsR a = do
  ns <- getNotifications a
  c <- snd <$> loadNotificationRelation a ns TM.Empty
  runViewM $ mapM (renderNotification c) ns

getMeR :: AccountId -> AppM MeResponse
getMeR a =
  MeResponse <$> getOwnerKey (unAccountId a)

getOwnersR :: AccountId -> ResourceId -> AppM [OwnerResponse]
getOwnersR (AccountId a) uid = do
  xs <- getOwners a uid
  runViewM $ mapM renderOwnerResponse xs

postOwnersR :: AccountId -> ResourceId -> String -> AppM ()
postOwnersR (AccountId a) u k = do
  liftIO $ putStrLn "test"
  addOwner a u k

deleteOwnersR :: AccountId -> ResourceId -> AppM ()
deleteOwnersR (AccountId a) = deleteOwner a
