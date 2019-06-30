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

getPosts :: MonadService m => AccountId -> m [Post]
getPosts aid = do
  uids <- queryM selectUserIds (unAccountId aid)
  uids' <- queryM selectOwnerFollowees (unAccountId aid)
  queryM (selectPosts (uids ++ uids')) ()

createPost :: MonadService m => AccountId -> ResourceId -> String -> [String] -> Maybe ResourceId -> m ResourceId
createPost aid uid body urls rep = do
  pid <- getTid
  let p = Post
        { id = pid
        , userId = uid
        , body = body
        , replyTo = rep
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

loadPostsRelation xs ctx =
  base xs ctx
  ->> loadPostReplies (Var :: Var "replies") ((^. #id) <$> xs)
  ->>= base
  where
    base cs ctx =
      loadPostImages (Var :: Var "postImages") ((^. #id) <$> xs) ctx
      ->> loadUser (Var :: Var "users") ((^. #userId) <$> xs)
      ->>= loadUserRelation
