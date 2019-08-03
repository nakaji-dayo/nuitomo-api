{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module Service.Post where

import           App
import           Data.Maybe
-- import           Data.Type.Map as TM
import           Auth
import           Control.Monad
import           Data.Type.Map        as TM
import           EagerLoader
import           Entity
import           Entity.Like
import           Entity.Notification
import           Entity.Post
import           Entity.PostImage
import           Query                as Q
import           Service.Loader
import           Service.Notification
import           Service.User
import           Service.Util
import           Type

getPosts :: MonadService m => AccountId -> Maybe ResourceId -> Maybe ResourceId -> Maybe String -> m [Post]
getPosts aid muid mcursor mscope = do
  uids <- case muid of
    Nothing -> do
      uids <- queryM selectUserIds (unAccountId aid)
      uids' <- case mscope of
        Just "home" -> pure []
        _           -> queryM selectOwnerFollowees (unAccountId aid)
      pure $ uids ++ uids'
    Just x -> pure [x]
  queryM (selectPosts uids mcursor) ()

getPost :: MonadService m => ResourceId -> m Post
getPost pid = getResource selectPost pid

createPost :: MonadService m => AccountId -> ResourceId -> String -> [String] -> Maybe ResourceId -> m ResourceId
createPost (AccountId aid) uid body imagePaths rep = do
  getOwnUser aid uid
  pid <- getTid
  now <- getCurrentLocalTime
  let p = Post
        { id = pid
        , userId = uid
        , body = body
        , replyTo = rep
        , mentionTo = Nothing
        , createdAt = now
        , aggLikeCount = 0
        , aggReplyCount = 0
        }
  pis <- forM imagePaths $ \x -> do
    iid <- getTid
    pure $ PostImage
      { id = iid
      , postId = pid
      , path = x
      }
  runTransactionM $ do
    insertM insertPost p
    bulkInsertM insertPostImage pis
    forM rep $ \r -> do
      p <- getResource selectPost r
      void $ createNotification  (p ^. #userId) NotifyReply Nothing (Just pid)
  forM rep $ updateM countReply
  return pid

loadPostsRelation (AccountId aid) xs ctx = do
  (us, ctx') <- loadPostImages (Var :: Var "postImages") ((^. #id) <$> xs) ctx
  (ps, ctx'') <- loadPostReplies (Var :: Var "replies") ((^. #id) <$> xs) ctx'
  let ps' = xs ++ ps
  loadPostImages (Var :: Var "postImages") ((^. #id) <$> ps') ctx''
    ->> loadPostLikes aid (Var :: Var "postLikes") ((^. #id) <$> ps')
    ->> loadUser (Var :: Var "users") ((^. #userId) <$> ps')
    ->>=loadUserRelation

createLike (AccountId i) uid pid = do
  getOwnUser i uid
  p <- getResource selectPost pid
  tid <- getTid
  now <- getCurrentLocalTime
  let l = Like
        { id = tid
        , postId = pid
        , userId = uid
        , createdAt = now
        }
  runTransactionM $ do
    insertM insertLike l
    void $ createNotification  (p ^. #userId) NotifyLike (Just uid) (Just pid)
  updateM countLike pid
  return tid

deleteLike (AccountId a) uid pid = do
  getOwnUser a uid
  deleteM Q.deleteLike (uid, pid)
  pure ()
