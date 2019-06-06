{-# LANGUAGE FlexibleContexts #-}
module View.Base where

import           App
import           Control.Monad.Catch (Exception, throwM)
import qualified Data.Map            as M
import           Data.Type.Map       as TM
import           GHC.TypeLits        (KnownSymbol)


data ViewError = ErrEagerLoading String | ErrJsonDecode
  deriving (Show)
instance Exception ViewError

get :: (KnownSymbol v, Ord k, IsMember v (M.Map k a) m) => Var v -> Map m -> k -> ViewM a
get key included rid = case M.lookup rid $ lookp key included of
  Just x  -> return x
  Nothing -> throwM $ ErrEagerLoading (show key)

getList :: (KnownSymbol v, Ord k, IsMember v (M.Map k [a]) m) => k -> Var v -> Map m -> ViewM [a]
getList rid key included = case M.lookup rid $ lookp key included of
  Just x  -> return x
  Nothing -> return []