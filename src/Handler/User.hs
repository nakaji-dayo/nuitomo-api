{-# LANGUAGE OverloadedLabels #-}
module Handler.User where

import           App
import           Auth               (AuthUser (..))
import           Data.Type.Map      as TM
import           Handler.Middleware
import           Service.User
import           Type               as T
import           View

getUsersR :: AuthUser -> AppM [UserResponse]
getUsersR au = do
  xs <- getUser (au ^. #sub)
  loaded <- snd <$> loadUserRelation xs TM.Empty
  runViewM $ mapM (renderUser loaded) xs

postUsersR :: AuthUser -> CreateUserRequest ->  AppM ResourceId
postUsersR au req =
  createUser (au ^. #sub) (req ^. #name) (req ^. #image)
