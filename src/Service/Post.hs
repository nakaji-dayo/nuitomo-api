{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module Service.Post where

import           App
-- import           Data.Type.Map as TM
import           Auth
import           Control.Monad
import           Data.Type.Map    as TM
import           EagerLoader
import           Entity
import           Entity.Post
import           Entity.PostImage
import           Query
import           Service.Loader
import           Service.User
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
createPost aid uid body urls rep = do
  pid <- getTid
  let p = Post
        { id = pid
        , userId = uid
        , body = body
        , replyTo = rep
        , mentionTo = Nothing
        }
  pis <- forM urls $ \x -> do
    iid <- getTid
    pure $ PostImage
      { id = iid
      , postId = pid
      , url = x
      }
  runTransactionM $ do
    insertM insertPost p
    bulkInsertM insertPostImage pis
  return pid

loadPostsRelation xs ctx = do
  (us, ctx') <- loadPostImages (Var :: Var "postImages") ((^. #id) <$> xs) ctx
  (ps, ctx'') <- loadPostReplies (Var :: Var "replies") ((^. #id) <$> xs) ctx'
  let ps' = xs ++ ps
  loadPostImages (Var :: Var "postImages") ((^. #id) <$> ps') ctx''
    ->> loadUser (Var :: Var "users") ((^. #userId) <$> ps')
    ->>=loadUserRelation
