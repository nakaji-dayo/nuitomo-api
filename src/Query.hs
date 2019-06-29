{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Query where

-- import           Data.Int
import           Control.Monad
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
--       pure u

-- selectTasks :: Query Int64 Task
-- selectTasks = relationalQuery' r []
--   where
--     r = relation' . placeholder $ \ph -> do
--       t <- query task
--       wheres $ t ! #userId .=. ph
--       pure t

q x = relationalQuery' (relation x) []
q' x = relationalQuery' (relation'. placeholder $ x) []

selectUsers :: Query String User
selectUsers =  q' $ \ph -> do
  u <- query E.user
  ou <- query ownerUser
  on $ u ! #id .=. ou ! #userId
  wheres $ ou ! #ownerId .=. ph
  pure u

selectFollowees :: Query ResourceId User
selectFollowees =  q' $ \ph -> do
  u <- query E.user
  f <- query follow
  on $ u ! #id .=. f ! #toUserId
  wheres $ f ! #userId .=. ph
  pure u

selectOwnerFollowees :: Query String ResourceId
selectOwnerFollowees =  q' $ \ph -> do
  ou <- query ownerUser
  f <- query follow
  on $ ou ! #userId .=. f ! #userId
  wheres $ ou ! #ownerId .=. ph
  pure $ f ! #toUserId

selectFollowers :: Query ResourceId User
selectFollowers =  q' $ \ph -> do
  u <- query E.user
  f <- query follow
  on $ u ! #id .=. f ! #userId
  wheres $ f ! #toUserId .=. ph
  pure u

selectOwnUser :: Query (String, ResourceId) User
selectOwnUser =  q' $ \ph -> do
  u <- query E.user
  ou <- query ownerUser
  on $ u ! #id .=. ou ! #userId
  wheres $ ou ! #ownerId .=. (ph ! fst')
  wheres $ u ! #id .=. (ph ! snd')
  pure u

selectUserSearch :: Maybe String -> Query () User
selectUserSearch mq = q $ do
  u <- query E.user
  forM_ mq $ \x -> wheres $ u ! #name `like'` value ("%" <> x <> "%")
  pure u

selectUserIds :: Query String ResourceId
selectUserIds =  q' $ \ph -> do
  ou <- query ownerUser
  wheres $ ou ! #ownerId .=. ph
  pure $ ou ! #userId

selectPosts :: [ResourceId] -> Query () Post
selectPosts uids = relationalQuery' r []
  where
    r = relation $ do
      p <- query post
      wheres $ p ! #userId `in'` values' uids
      desc $ p ! #id
      pure p


deleteFollow :: Delete (ResourceId, ResourceId)
deleteFollow = delete $ \proj ->
  fmap fst . placeholder $ \ph -> do
    wheres $ (proj :: Record Flat Follow) ! #userId .=. (ph ! fst')
    wheres $ proj ! #toUserId .=. (ph ! snd')


include e ids = relation $ do
  r <- query e
  wheres $ r ! #id `in'` values' ids
  pure $ (r ! #id) >< r

-- todo: 整理
makeInclude ::
  (PersistableWidth b, LiteralSQL a,
   Num a) =>
  Relation () b -> Pi b a -> [a] -> Relation () (a, b)
makeInclude t k ids = relation $ do
  u <- query t
  wheres $ u ! k `in'` values' ids
  pure $ (u ! k) >< u

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
  pure $ (a ! k) >< b

-- includeUsers = makeInclude

includeUserImages :: [ResourceId] -> Relation () (ResourceId, UserImage)
includeUserImages = makeInclude userImage #userId

includePostImages = makeInclude postImage #postId
