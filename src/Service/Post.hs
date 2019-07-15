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
import           Data.Type.Map       as TM
import           EagerLoader
import           Entity
import           Entity.Like
import           Entity.Notification
import           Entity.Post
import           Entity.PostImage
import           Query               as Q
import           Service.Loader
import           Service.User
import           Service.Util
import           Type

getPosts :: MonadService m => AccountId -> Maybe ResourceId -> m [Post]
getPosts aid muid = do
  uids <- case muid of
    Nothing -> do
      uids <- queryM selectUserIds (unAccountId aid)
      uids' <- queryM selectOwnerFollowees (unAccountId aid)
      pure $ uids ++ uids'
    Just x -> pure [x]
  queryM (selectPosts uids) ()

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
  return pid

loadPostsRelation (AccountId aid) xs ctx = do
  (us, ctx') <- loadPostImages (Var :: Var "postImages") ((^. #id) <$> xs) ctx
  (ps, ctx'') <- loadPostReplies (Var :: Var "replies") ((^. #id) <$> xs) ctx'
  let ps' = xs ++ ps
  loadPostImages (Var :: Var "postImages") ((^. #id) <$> ps') ctx''
    ->> loadPostLikes aid (Var :: Var "postLikes") ((^. #id) <$> ps')
    ->> loadUser (Var :: Var "users") ((^. #userId) <$> ps')
    ->>=loadUserRelation

createNotification uid nt mruid mrpid = do
  tid <- getTid
  now <- getCurrentLocalTime
  let n = Notification
        { id = tid
        , userId = uid
        , notificationType = fromEnum' nt
        , refUserId = mruid
        , refPostId = mrpid
        , createdAt = now
        }
  insertM insertNotification n

createLike (AccountId i) uid pid = do
  getOwnUser i uid
  p <- getResource selectPost pid
  tid <- getTid
  let l = Like
        { id = tid
        , postId = pid
        , userId = uid
        }
  runTransactionM $ do
    insertM insertLike l
    void $ createNotification  (p ^. #userId) NotifyLike (Just uid) (Just pid)
  return tid

deleteLike (AccountId a) uid pid = do
  getOwnUser a uid
  deleteM Q.deleteLike (uid, pid)
  pure ()
