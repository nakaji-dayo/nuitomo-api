{-# LANGUAGE OverloadedLabels #-}
module Handler.Post where

import           App
import           Auth
import           Data.Type.Map      as TM
import           EntityId
import           Handler.Middleware
import           Service.Post
import           Type               as T
import           View

getPostsR :: AccountId -> Maybe UserId -> Maybe PostId -> Maybe String -> AppM [PostResponse]
getPostsR aid muid mcursor mscope = do
  xs <- getPosts aid muid mcursor mscope
  c <- snd <$> loadPostsRelation aid xs TM.Empty
  runViewM $ mapM (renderPost c) xs

getPostR :: AccountId -> PostId -> AppM PostResponse
getPostR aid pid = do
  x <- getPost pid
  c <- snd <$> loadPostsRelation aid [x] TM.Empty
  runViewM $ renderPost c x

postPostsR :: AccountId -> CreatePostRequest -> AppM PostId
postPostsR i req =
  createPost i (req ^. #userId) (req ^. #body) (req ^. #images) (req ^. #replyTo)

postLikesR :: AccountId -> CreateLikeRequest -> AppM LikeId
postLikesR i req = createLike i (req ^. #userId) (req ^. #postId)

deleteLikesR :: AccountId -> CreateLikeRequest -> AppM ()
deleteLikesR i req = deleteLike i (req ^. #userId) (req ^. #postId)
