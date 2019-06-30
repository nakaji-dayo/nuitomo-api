-- Todo: 要整理、ライブラリ化
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module EagerLoader where

import           App
import qualified Data.Map                           as M
import           Data.Maybe
import           Data.Type.Map                      as TM
import           Database.Relational.Monad.BaseType (Relation)
import           Util

type TMap = M.Map

-- (->>) :: (Monad m, MonadService m) => m (a, t) -> (t -> m b) -> m b
(->>) :: Monad m => m (a, t) -> (t -> m b) -> m b
f ->> g = f >>= \(_, b) -> g b

-- (->>=) :: Monad m => m (a, b) -> (a -> b -> m b1) -> m b1
(->>=) f g = f >>= uncurry g

load ::
  (FromSql SqlValue k1, FromSql SqlValue a, MonadService m1,
   Ord k1) =>
  (t -> Relation () (k1, a))
  -> Var k2
  -> t
  -> Map m2
  -> m1 ([a], Map ((k2 ':-> M.Map k1 a) : m2))
load query key ids m = do
  rs <- queryM (relationalQuery' (query ids) []) ()
  return (snd <$> rs, Ext key (M.fromList rs) m)

loadList ::
  (FromSql SqlValue a, FromSql SqlValue b, MonadService m1, Ord a) =>
  (t -> Relation () (a, b))
  -> Var k
  -> t
  -> Map m2
  -> m1 ([b], Map ((k ':-> M.Map a [b]) : m2))
loadList query key ids m = do
  rs <- queryM (relationalQuery' (query ids) []) ()
  return (snd <$> rs, Ext key (createMapWithListValue rs) m)

loadList' query key ids m = do
  rs <- fmap (mapFst fromJust) <$> queryM (relationalQuery' (query ids) []) ()
  return (snd <$> rs, Ext key (createMapWithListValue rs) m)

type MkLoader ek r e = forall m k v . MonadService m => Var k -> [ek] -> Map v -> m (r, Map ((k ':-> M.Map ek e) : v))

type Loader k a = MkLoader k [a] a
type ListLoader k a = MkLoader k [a] [a]
