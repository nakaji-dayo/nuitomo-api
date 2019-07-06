{-# LANGUAGE OverloadedLabels #-}
module Handler.User where

import           App
import           Auth
import           Data.Type.Map      as TM
import           Handler.Middleware
import           Service.User
import           Type               as T
import           View

loadRenderUsers xs =  do
  loaded <- snd <$> loadUserRelation xs TM.Empty
  runViewM $ mapM (renderUser loaded) xs

getUsersR :: AccountId -> AppM [UserResponse]
getUsersR i = getUsers i >>= loadRenderUsers

getUsersSearchR :: AccountId -> Maybe String -> AppM [UserResponse]
getUsersSearchR _ q = searchUser q >>= loadRenderUsers

getUserR :: AccountId -> ResourceId -> AppM UserResponse
getUserR _ uid = do
  x <- getUser uid
  loaded <- snd <$> loadUserRelation [x] TM.Empty
  runViewM $ renderUser loaded x

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
