{-# LANGUAGE OverloadedLabels #-}
module Handler.Post where

import           App
import           Auth               (AuthUser (..))
import           Data.Type.Map      as TM
import           Handler.Middleware
import           Service.Post
import           Type               as T
import           View

getPostsR :: AuthUser -> AppM [PostResponse]
getPostsR _ = do
  xs <- getPosts
  -- loaded <- snd <$> loadPostsRelation xs TM.Empty
  runViewM $ mapM renderPost xs

postPostsR :: AuthUser -> CreatePostRequest -> AppM ResourceId
postPostsR u req =
  createPost (u ^. #sub) (req ^. #userId) (req ^. #body)
