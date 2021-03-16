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
import           Entity.QuestionUser
import           EntityId
import           Query                as Q
import           Service.Loader
import           Service.Notification
import           Service.User
import           Service.Util
import           System.Random
import           Type

getPosts :: MonadService m => AccountId -> Maybe UserId -> Maybe PostId -> Maybe String -> m [Post]
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

getPost :: MonadService m => PostId -> m Post
getPost pid = getResource selectPost pid

createPost :: MonadService m => AccountId -> UserId -> String -> [String] -> Maybe PostId -> m PostId
createPost (AccountId aid) uid body imagePaths rep = do
  getOwnUser aid uid
  pid <- PostId <$> getTid
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
    iid <- PostImageId <$> getTid
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
  tid <- LikeId <$> getTid
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

createQuestionPost :: MonadService m => UserId -> m ()
createQuestionPost uid = do
  qs <- queryM selectNewQuestions uid
  case qs of
    h:_ -> do
      let qs' = filter (\x -> x ^. #priority == (h ^. #priority)) qs
      q <- liftIO $ atRandIndex qs'
      create q
    _ -> do
      liftIO $ putStrLn "all question has been used"
      pure ()
  where
    create q = do
      pid <- PostId <$> getTid
      quid <- QuestionUserId <$> getTid
      now <- getCurrentLocalTime
      let p = Post
            { id = pid
            , userId = robotUserId
            , body = q ^. #body
            , replyTo = Nothing
            , mentionTo = Just uid
            , createdAt = now
            , aggLikeCount = 0
            , aggReplyCount = 0
            }
      let qu = QuestionUser
            { id = quid
            , questionId = q ^. #id
            , userId = uid
            , createdAt = now
            }
      runTransactionM $ do
        insertM insertPost p
        insertM insertQuestionUser qu
        createNotification uid NotifyMention (Just robotUserId) (Just pid)
      pure ()


atRandIndex :: [a] -> IO a
atRandIndex l = do
    i <- randomRIO (0, length l - 1)
    return $ l !! i
