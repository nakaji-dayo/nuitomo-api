{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

module App (
 module App
 , NoContent(..)
 , relationalQuery'
 , liftIO
 , ask
 , asks
 , Connection
 , module Servant
 , module Logger
 , ToSql
 , SqlValue
 , FromSql
 , Query
 , Katip
 , KatipContext
) where

import           Config
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch         (Exception, MonadCatch, MonadThrow,
                                              SomeException (..), catch,
                                              onException, throwM)
import           Control.Monad.Error.Class
import           Control.Monad.Except        (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Int                    (Int64)
import           Database.Relational.Type    as RT
import           DataSource
import           Servant                     hiding (Context, serve)

-- DB
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Time.Clock             (UTCTime)
import           Data.Time.Clock.POSIX       (getPOSIXTime,
                                              posixSecondsToUTCTime,
                                              utcTimeToPOSIXSeconds)
import           Database.HDBC               (SqlValue, commit, rollback, run)
import           Database.HDBC.PostgreSQL    (Connection)
import qualified Database.HDBC.Record        as HDBC
import           Database.Record             (FromSql, ToSql)
import           Database.Relational         (Query, relationalQuery')
-- import qualified Network.Google              as Google
-- import qualified Network.Google.Auth         as Google
-- Logger
import qualified Data.Text                   as T
import           Katip
import           System.IO                   (stdout)
-- import           Text.Printf.TH              (st)
-- import qualified DB.Error                    as DBE
import qualified Data.Time.Clock             as Time
import           Logger
import           Network.HTTP.Conduit        (Manager, newManager,
                                              tlsManagerSettings)
-- stm
import qualified Control.Concurrent.STM      as STM

-- type AppGoogleScopes = '["https://www.googleapis.com/auth/androidpublisher", "https://www.googleapis.com/auth/firebase.messaging"]

initialize :: IO (Context, LogEnv)
initialize = do
  config <- Config.loadConfig
  logEnv <- loadAppLogEnv config
  pool <- createPool' config
  let dbConnection = Nothing
  httpClientManager <- newDefaultHttpManager
  tidVar <- newTidVar
  return (Context {..}, logEnv)

data Context = Context
  { config            :: Config
  , pool              :: Pool Connection
  -- , googleEnv         :: Google.Env AppGoogleScopes
  -- , googlePlayEnv     :: Google.Env AppGoogleScopes
--  , firebaseEnv       :: Firebase.FirebaseEnv
  , dbConnection      :: Maybe Connection
  , httpClientManager :: Manager
  , tidVar            :: STM.TVar (Int64, Int)
--  , jwtCfg            :: JWTSettings
  }

newTidVar :: IO (STM.TVar (Int64, Int))
newTidVar = STM.newTVarIO (0, 0)

class SystemIO m where
  getCurrentTime :: m Time.UTCTime

class (MonadReader Context m, SystemIO m) => MonadView m
newtype ViewM a = ViewM { unViewM :: ReaderT Context IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadThrow
    , MonadReader Context
    )

instance SystemIO ViewM where
  getCurrentTime = ViewM . liftIO $ Time.getCurrentTime
instance SystemIO AppM where
  getCurrentTime = liftIO Time.getCurrentTime
instance MonadView ViewM

class (Monad m, MonadReader Context m, MonadIO m, KatipContext m,
       MonadBaseControl IO m, MonadCatch m, MonadView m) => MonadService m where
  queryM :: (ToSql SqlValue p, FromSql SqlValue a, Show p, Eq p)
    => Query p a -> p -> m [a]
  execM :: String -> m Integer
  insertM :: (ToSql SqlValue a, Show a, Eq a) => Insert a -> a -> m Integer
  insertQueryM :: (ToSql SqlValue p, Show p, Eq p) => InsertQuery p -> p -> m Integer
  deleteM :: (ToSql SqlValue p, Show p, Eq p) => RT.Delete p -> p -> m Integer
  updateM :: (ToSql SqlValue p, Show p, Eq p) => Update p -> p -> m Integer
  keyUpdateM :: (ToSql SqlValue a, Show a, Eq a) => KeyUpdate p a -> a -> m Integer
  bulkInsertM :: (ToSql SqlValue a, Show a, Eq a) => Insert a -> [a] -> m ()
  getTid :: m Int64

newtype AppM a = AppM
  { runAppM :: ReaderT Context (KatipContextT Handler) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadError ServantErr
  , MonadReader Context
  , MonadThrow
  , MonadCatch
  , MonadBase IO
  , MonadBaseControl IO
  , Katip
  , KatipContext)

instance MonadView AppM

instance MonadService AppM where
  queryM r v =
    withResource' $ \conn -> liftIO $ HDBC.runQuery' conn r v
  execM s = withResource' $ \c -> liftIO $ Database.HDBC.run c s []
  insertM a b = _withResourceTransaction $ \conn -> liftIO $ HDBC.runInsert conn a b
  insertQueryM a b = _withResourceTransaction $ \conn -> liftIO $ HDBC.runInsertQuery conn a b
  deleteM a b = _withResourceTransaction $ \conn -> liftIO $ HDBC.runDelete conn a b
  updateM a b = _withResourceTransaction $ \conn -> liftIO $ HDBC.runUpdate conn a b
  keyUpdateM a b = _withResourceTransaction $ \conn -> liftIO $ HDBC.runKeyUpdate conn a b
  bulkInsertM a b = _withResourceTransaction $ \conn -> liftIO $ HDBC.bulkInsert conn a b
  getTid = do
    var <- asks tidVar
    liftIO $ do
      t <- round . (* 100) <$> getPOSIXTime
      c <- next var t
      when (c >= 100) $ throwM TidDepleted
      return $ t * 100 + fromIntegral c
    where
      next var t = STM.atomically $ do
        (t', c') <- STM.readTVar var
        case t == t' of
          True -> do
            STM.writeTVar var (t, c' + 1)
            return c'
          False -> do
            STM.writeTVar var (t, 1)
            return 0

nt :: Context -> (LogEnv, T.Text) -> AppM a -> Handler a
nt c (logEnv, namespace) (AppM m) = runKatipContextT logEnv () (Namespace [namespace]) (runReaderT m c)

-- ioLogger ns logEnv l b = do
--   let l' = case l of
--         Google.Info  -> InfoS
--         Google.Error -> ErrorS
--         Google.Debug -> DebugS
--         Google.Trace -> DebugS
--   runKatipContextT logEnv () (Namespace [ns]) $
--     logFM l' $ logStr (toLazyByteString b)

-- makeGoogleEnv :: LogEnv -> String -> IO (Google.Env AppGoogleScopes)
-- makeGoogleEnv logEnv creds = do
--   creds <- Google.fromFilePath creds
--   mgr <- liftIO $ newManager Google.tlsManagerSettings
--  --  lgr <- Google.newLogger Google.Trace stdout
--   Google.newEnvWith
--     creds
--     (ioLogger "google" logEnv)
--     mgr

newDefaultHttpManager :: IO Manager
newDefaultHttpManager = newManager tlsManagerSettings

runViewM :: MonadService m => ViewM a -> m a
runViewM a = do
  c <- ask
  liftIO $ runReaderT (unViewM a) c

-- Logger
loadAppLogEnv :: Config -> IO LogEnv
loadAppLogEnv cfg = do
  logger <- initLogEnv "LEMO" (Environment . T.pack . show $ deployEnv cfg)
  scribe <- mkHandleScribe ColorIfTerminal stdout (logLevel cfg) V2
  registerScribe "stdout" scribe defaultScribeSettings logger

-- DB Utils

data AppException = TidDepleted
  deriving (Show, Eq)
instance Exception AppException

tidTime :: Int64 -> UTCTime
tidTime x = posixSecondsToUTCTime (fromIntegral x / 100 / 100)

timeTid0 :: UTCTime -> Int64
timeTid0 x = floor $ utcTimeToPOSIXSeconds x * 100 * 100

getTid2 :: MonadService m => m (Int64, Int64)
getTid2 = pure (,) <*> getTid <*> getTid

getTid3 :: MonadService m => m (Int64, Int64, Int64)
getTid3 = pure (,,) <*> getTid <*> getTid <*> getTid

timeToTid :: UTCTime -> Int64
timeToTid x = round . (* (100 * 100)) $ utcTimeToPOSIXSeconds x

withTransaction' :: (MonadIO m, MonadCatch m) => Connection  -> (Connection -> m a) -> m a
withTransaction' conn func = do
  r <- onException (func conn) doRollback
  liftIO $ commit conn
  return r
  where
    doRollback = catch (liftIO $ rollback conn) doRollbackHandler
    doRollbackHandler :: (MonadIO m, MonadCatch m) => SomeException -> m ()
    doRollbackHandler _ = return ()

withResource' :: MonadService m => (Connection -> m b) -> m b
withResource' op = do
  cfg <- ask
  case dbConnection cfg of
    Just c  -> op c
    Nothing ->
      withResource (pool cfg) op

_withResourceTransaction ::
  (MonadService m) =>
  (Connection -> m b) -> m b
_withResourceTransaction sub = do
  cfg <- ask
  case dbConnection cfg of
    Just c  -> sub c
    Nothing -> withResource (pool cfg) $ \conn -> withTransaction' conn sub

runTransactionM :: MonadService m => m a -> m a
runTransactionM operation =
  _withResourceTransaction $ \conn -> local (\c -> c { dbConnection = Just conn }) operation

selectOneM :: (ToSql SqlValue p, FromSql SqlValue a, MonadService m, Show p, Eq p) => Query p a -> p -> m (Maybe a)
selectOneM r v = do
  xs <- queryM r v
  return $ case xs of
    x:_ -> Just x
    _   -> Nothing

mustFoundM :: Maybe a -> AppM a
mustFoundM = maybe (throwError err404) return

mustSelectOneM :: (ToSql SqlValue p, FromSql SqlValue a, Show p, Eq p) => Query p a -> p -> AppM a
mustSelectOneM r v = selectOneM r v >>= mustFoundM

fromEnum' :: (Integral n, Enum e) => e -> n
fromEnum' = fromIntegral . fromEnum

toEnum' :: (Integral n, Enum e) => n -> e
toEnum' = toEnum . fromIntegral