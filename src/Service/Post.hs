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
import qualified Query            as Q
import           Service.Loader
import           Service.User
import           Type

getPosts :: MonadService m => m [Post]
getPosts =
  queryM Q.selectPosts ()

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
  loadPostImages (Var :: Var "postImages") ((^. #id) <$> xs) ctx
   ->> loadUser (Var :: Var "users") ((^. #userId) <$> xs)
  ->>= loadUserRelation
