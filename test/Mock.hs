-- todo: 整理
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
module Mock
  ( module Mock
  , runMockT
  , WithResult (..)
  ) where

import           App
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch         (MonadCatch, MonadThrow)
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.Mock
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Text                   as T
import           Data.Time.Clock.POSIX
import           Data.Type.Equality
import           Database.Relational         as R
import           DataSource
import           GHC.Int
import           Katip
import           Unsafe.Coerce

newtype TestM a = TestM
  { unTestM :: AppM a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader Context
  , MonadThrow
  , MonadCatch
  , MonadBase IO
  , MonadBaseControl IO
  , Katip
  , KatipContext
  )

runTestM :: TestM a -> IO a
runTestM h = do
  (ctx, logEnv) <- initialize
  Right r <- runExceptT $ runHandler' $ nt' ctx (logEnv, "RunAppM") h
  destroyAllResources (pool ctx)
  return r

nt' :: Context -> (LogEnv, T.Text) -> TestM a -> Handler a
nt' c (logEnv, namespace) (TestM (AppM m)) = runKatipContextT logEnv () (Namespace [namespace]) (runReaderT m c)

instance SystemIO TestM where
  getCurrentTime = undefined
instance MonadView TestM

instance SystemIO (MockT a TestM) where
  getCurrentTime = liftIO Data.Time.Clock.POSIX.getCurrentTime
instance MonadView (MockT a TestM)
instance Katip (MockT a TestM) where
  getLogEnv = lift getLogEnv
  localLogEnv = localLogEnv
instance KatipContext (MockT a TestM) where
  getKatipContext = lift getKatipContext
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = localKatipNamespace
  localKatipContext = localKatipContext

-- 軽く試した範囲だとmakeActionで生成できない
-- https://github.com/cjdev/monad-mock/issues/4
data ServiceAction m where
  QueryM :: (Eq p, Show p) => Query p a -> p -> ServiceAction [a]
  ExecM :: String -> ServiceAction Integer
  InsertM :: (Show a, Eq a) => Insert a -> a -> ServiceAction Integer
  InsertQueryM :: (Show p, Eq p) => InsertQuery p -> p -> ServiceAction Integer
  DeleteM :: (Show p, Eq p) => R.Delete p -> p -> ServiceAction Integer
  UpdateM :: (Show p, Eq p) => Update p -> p -> ServiceAction Integer
  KeyUpdateM :: (Show a, Eq a) => KeyUpdate p a -> a -> ServiceAction Integer
  BulkInsertM :: (Show a, Eq a) => Insert a -> [a] -> ServiceAction ()
  GetTid :: ServiceAction Int64

deriving instance Show (ServiceAction m)
-- deriving instance Eq (ServiceAction m)
instance Action ServiceAction where
  eqAction (QueryM a b) (QueryM a' b')
    = if _eq a a' b b'
      then Just (unsafeCoerce Refl) else Nothing
  eqAction (ExecM a) (ExecM a')
    = if a == a' then Just Refl else Nothing
  eqAction (InsertM a b) (InsertM a' b')
    = if _eq a a' b b'
      then Just (unsafeCoerce Refl) else Nothing
  eqAction (InsertQueryM a b) (InsertQueryM a' b')
    = if _eq a a' b b'
      then Just (unsafeCoerce Refl) else Nothing
  eqAction (DeleteM a b) (DeleteM a' b')
    = if _eq a a' b b'
      then Just (unsafeCoerce Refl) else Nothing
  eqAction (UpdateM a b) (UpdateM a' b')
    = if _eq a a' b b'
      then Just (unsafeCoerce Refl) else Nothing
  eqAction (KeyUpdateM a b) (KeyUpdateM a' b')
    = if _eq a a' b b'
      then Just (unsafeCoerce Refl) else Nothing
  eqAction (BulkInsertM a b) (BulkInsertM a' b')
    = if _eq a a' b b'
      then Just (unsafeCoerce Refl) else Nothing
  eqAction GetTid GetTid = Just Refl
  eqAction _ _ = Nothing

_eq :: (Show a1, Show a2, Eq a3) => a1 -> a2 -> a3 -> a4 -> Bool
_eq a a' b b' = show a == show a' && b == unsafeCoerce b'

instance (Monad m, MonadReader Context m, MonadIO m, KatipContext (MockT ServiceAction m),
       MonadBaseControl IO m, MonadCatch m, MonadView (MockT ServiceAction m)) => MonadService (MockT ServiceAction m) where
  queryM a b = mockAction "queryM" (QueryM a b)
  execM a = mockAction "execM" (ExecM a)
  insertM a b = mockAction "insertM" (InsertM a b)
  insertQueryM a b = mockAction "insertQueryM" (InsertQueryM a b)
  deleteM a b = mockAction "deleteM" (DeleteM a b)
  updateM a b = mockAction "updateM" (UpdateM a b)
  keyUpdateM a b = mockAction "keyUpdateM" (KeyUpdateM a b)
  bulkInsertM a b = mockAction "bulkInsertM" (BulkInsertM a b)
  getTid = mockAction "getTid" GetTid