{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Query where

-- import           Data.Int
import           Database.Record.Persistable (PersistableWidth)
import           Database.Relational
import           Entity                      as E
import           Query.Util
import           Type                        as T

-- selectUser' :: Query Int64 User
-- selectUser' = relationalQuery' r []
--   where
--     r = relation' . placeholder $ \ph -> do
--       u <- query user
--       wheres $ u ! #id .=. ph
--       return u

-- selectTasks :: Query Int64 Task
-- selectTasks = relationalQuery' r []
--   where
--     r = relation' . placeholder $ \ph -> do
--       t <- query task
--       wheres $ t ! #userId .=. ph
--       return t

q x = relationalQuery' (relation x) []
q' x = relationalQuery' (relation'. placeholder $ x) []

selectUsers :: Query String User
selectUsers =  q' $ \ph -> do
  u <- query E.user
  ou <- query ownerUser
  wheres $ ou ! #ownerId .=. ph
  return u


selectPosts :: Query () Post
selectPosts = relationalQuery' r []
  where
    r = relation $ do
      p <- query post
      desc $ p ! #id
      return p

include e ids = relation $ do
  r <- query e
  wheres $ r ! #id `in'` values' ids
  return $ (r ! #id) >< r

-- todo: 整理
makeInclude ::
  (PersistableWidth b, LiteralSQL a,
   Num a) =>
  Relation () b -> Pi b a -> [a] -> Relation () (a, b)
makeInclude t k ids = relation $ do
  u <- query t
  wheres $ u ! k `in'` values' ids
  return $ (u ! k) >< u

make1NInclude
  :: (LiteralSQL a1, Num a1, PersistableWidth a, PersistableWidth b)
  => Relation () a
  -> Pi a ft
  -> Relation () b
  -> Pi b ft
  -> Pi a a1
  -> Pi a t
  -> [a1]
  -> Relation () (a1, b)
make1NInclude t1 c1 t2 c2 k ordC ids = relation $ do
  a <- query t1
  b <- query t2
  on $ a ! c1 .=. b ! c2
  wheres $ a ! k `in'` values' ids
  asc $ a ! ordC
  return $ (a ! k) >< b

-- includeUsers = makeInclude

includeUserImages :: [ResourceId] -> Relation () (ResourceId, UserImage)
includeUserImages = makeInclude userImage #userId

includePostImages = makeInclude postImage #postId
