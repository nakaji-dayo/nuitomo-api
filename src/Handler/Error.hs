-- todo: 再設計
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Handler.Error where

import           App
import           Control.Lens
import           Control.Monad.Catch (handle)
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Types  (hContentType)
import           Servant.Auth.Server (ThrowAll, throwAll)
import           Service.Exception   as E
import           TH.Type
import           Util

data ErrorBody = ErrorBody
  { code      :: String
  , message   :: String
  , messageJa :: Maybe String
  , fields    :: Maybe [String]
  , reason    :: Maybe [String]
  } deriving (Show, Generic, Eq)
$(deriveApiField ''ErrorBody)

data AppError = AppError
  { err  :: ServantErr
  , body :: ErrorBody
  } deriving (Show, Generic, Eq)
makeLenses ''AppError

eb :: ServantErr -> String -> String -> String -> AppError
eb e c msg ja = AppError e $ ErrorBody c msg (Just ja) Nothing Nothing

handleGeneralException :: AppM a -> AppM a
handleGeneralException = handle generalHandler

errZ001 :: AppError
errZ001 = eb err404 "Z001" "Unauthorized" "権限がありません"

errR000 :: AppError
errR000 = eb err400 "R000" "Resource Not Exists" "リソースが見つかりません"

errR001 :: AppError
errR001 = eb err400 "R001" "Resource Create Rejected" "リソースを作成できません"

errR006 :: AppError
errR006 = eb err409 "R006" "Resource Conflict" "リソースが競合しています。"

errE000 :: AppError
errE000 = eb err500 "E000" "System Error" "エラーが発生しました"

generalHandler :: ServiceException  -> AppM a
generalHandler ex = do
  case ex of
    OtherException e -> $(logErrorM) $ "Unknown Error: " `append` tshow e
    e                ->   $(logInfoM) $ "WellKnown Error: " `append` tshow e
  throwJSONError (h ex)
  where
    -- 認証
    h E.Unauthorized     = errZ001
    h ResourceNotExist   = errR000
    h ResourceConflict   = errR006
    h (OtherException _) = errE000

formatError :: AppError -> ServantErr
formatError (AppError e b) = e
  { errBody = encode b
  , errHeaders = [ jsonHeader ]
  }
  where
    jsonHeader = ( hContentType
                 , "application/json;charset=utf-8" )

throwJSONError :: AppError -> AppM a
throwJSONError = throwError . formatError

throwAllJSONError :: (ThrowAll a) => AppError -> a
throwAllJSONError = throwAll . formatError