{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Query where

import           Control.Monad
import           Data.Functor.ProductIsomorphic.Class
import           Database.Record.Persistable              (PersistableWidth)
import           Database.Relational
import           Database.Relational.OverloadedProjection (HasProjection)
import           Entity                                   as E
import           Entity.Post                              as Post
import           Query.Util
import           Type                                     as T

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

q :: QuerySimple (Record Flat r) -> Query () r
q x = relationalQuery' (relation x) []

q' ::
  (PersistableWidth p, SqlContext c) =>
  (Record c p -> Orderings Flat QueryCore (Record Flat r))
  -> Query p r
q' x = relationalQuery' (relation'. placeholder $ x) []

selectPrimaryOwner :: Query (String, ResourceId) ResourceId
selectPrimaryOwner = q' $ \ph -> do
  ou <- query ownerUser
  wheres $ ou ! #ownerId .=. ph ! fst'
  wheres $ ou ! #userId .=. ph ! snd'
  wheres $ ou ! #isPrimary .=. value True
  pure $ ou ! #id

selectOwnerKey :: Query String OwnerKey
selectOwnerKey = q' $ \ph -> do
  o <- query ownerKey
  wheres $ o ! #ownerId .=. ph
  pure o

selectOwners :: Query ResourceId (OwnerKey, OwnerUser)
selectOwners = q' $ \ph -> do
  ou <- query ownerUser
  o <- query ownerKey
  on $ ou ! #ownerId .=. o ! #ownerId
  wheres $ ou ! #userId .=. ph
  pure $ o >< ou

selectOwnerByKey :: Query String String
selectOwnerByKey = q' $ \ph -> do
  o <- query ownerKey
  wheres $ o ! #key .=. ph
  pure $ o ! #ownerId

deleteOwnerUser :: Delete ResourceId
deleteOwnerUser = delete $ \proj ->
  fmap fst . placeholder $ \ph -> do
    wheres $ (proj :: Record Flat OwnerUser) ! #id .=. ph

selectUser
  :: Query (String, ResourceId) (User, Maybe OwnerUser)
selectUser = q' $ \ph -> do
  u <- query E.user
  ou <- queryMaybe ownerUser
  on $ just (u ! #id) .=. ou ?! #userId
  wheres $ ou ?! #ownerId .=. just (ph ! fst')
  wheres $ u ! #id .=. ph ! snd'
  pure $ u >< ou


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
selectPosts uids = q $ do
  p <- query post
  wheres $ p ! #userId `in'` values' uids
  wheres $ isNothing (p ! #replyTo)
  desc $ p ! #id
  pure p


deleteFollow :: Delete (ResourceId, ResourceId)
deleteFollow = delete $ \proj ->
  fmap fst . placeholder $ \ph -> do
    wheres $ (proj :: Record Flat Follow) ! #userId .=. (ph ! fst')
    wheres $ proj ! #toUserId .=. (ph ! snd')

selectNotifications ::
  Query String (Notification, User, Maybe User, Maybe Post)
selectNotifications = q' $ \ph -> do
  n <- query notification
  ou <- query ownerUser
  on $ n ! #userId .=. ou ! #userId
  u <- query E.user
  on $ u ! #id .=. n ! #userId
  mpu <- queryMaybe E.user
  on $ mpu ?! #id .=. n ! #refUserId
  mp <- queryMaybe post
  on $ mp ?! #id .=. n ! #refPostId
  wheres $ ou ! #ownerId .=. ph
  desc $ n ! #id
  return $ (,,,) |$| n |*| u |*| mpu |*| mp

deleteLike :: Delete (ResourceId, ResourceId)
deleteLike = delete $ \proj ->
  fmap fst . placeholder $ \ph -> do
    wheres $ (proj :: Record Flat Like) ! #userId .=. (ph ! fst')
    wheres $ proj ! #postId .=. (ph ! snd')

countLike :: Update ResourceId
countLike = update $ \proj -> do
  fmap fst $  placeholder $ \ph -> do
    ts <- queryScalar $ aggregatedUnique ( relation $ do
                                             l <- query E.like
                                             wheres $ l ! #postId .=. proj ! #id
                                             return $ l ! #id
                                         ) Database.Relational.id' count
    Post.aggLikeCount' <-# fromMaybe' (value 0) ts
    wheres $ proj ! #id .=. ph

countReply :: Update ResourceId
countReply = update $ \proj -> do
  fmap fst $  placeholder $ \ph -> do
    ts <- queryScalar $ aggregatedUnique ( relation $ do
                                             p <- query E.post
                                             wheres $ p ! #replyTo .=. just (proj ! #id)
                                             wheres $ isJust (p ! #replyTo)
                                             return $ p ! #id
                                         ) Database.Relational.id' count
    Post.aggReplyCount' <-# fromMaybe' (value 0) ts
    wheres $ proj ! #id .=. ph

fromMaybe' :: SqlContext c => Record c a -> Record c (Maybe a) -> Record c a
fromMaybe' d m = unsafeProjectSql $ "COALESCE(" <> unsafeShowSql m <> ", " <> unsafeShowSql d <>")"

include ::
  (HasProjection "id" b a,
   PersistableWidth b, LiteralSQL a, Num a) =>
  Relation () b -> [a] -> Relation () (a, b)
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

includePostImages :: [ResourceId] -> Relation () (ResourceId, PostImage)
includePostImages = makeInclude postImage #postId

includePostReplies
  :: [ResourceId]
     -> Relation () (Maybe ResourceId, Post)
includePostReplies ids = relation $ do
  p <- query post
  wheres $ p ! #replyTo `in'` (values'' (Just <$> ids))
  wheres $ isJust (p ! #replyTo)
  pure $ (p ! #replyTo) >< p
  where
    values'' [] = values [Just (-1)]
    values'' xs = values xs

includePostLikes :: String -> [ResourceId] -> Relation () (ResourceId, Like)
includePostLikes aid ids = relation $ do
  l <- query E.like
  ou <- query ownerUser
  on $ l ! #userId .=. ou ! #userId
  wheres $ ou ! #ownerId .=. value aid
  wheres $ l ! #postId `in'` values' ids
  pure $ (l ! #postId) >< l
