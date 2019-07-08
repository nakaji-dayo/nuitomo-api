{-# LANGUAGE OverloadedLabels #-}
module Handler.Post where

import           App
import           Auth
import           Data.Type.Map      as TM
import           Handler.Middleware
import           Service.Post
import           Type               as T
import           View

getPostsR :: AccountId -> Maybe ResourceId -> AppM [PostResponse]
getPostsR aid muid = do
  xs <- getPosts aid muid
  c <- snd <$> loadPostsRelation xs TM.Empty
  runViewM $ mapM (renderPost c) xs

postPostsR :: AccountId -> CreatePostRequest -> AppM ResourceId
postPostsR i req =
  createPost i (req ^. #userId) (req ^. #body) (req ^. #images) (req ^. #replyTo)
