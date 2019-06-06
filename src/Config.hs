{-# LANGUAGE DeriveGeneric #-}
module Config where

import           Data.Aeson
import qualified Data.Yaml.Config as C
import           GHC.Generics
import           Katip            (Severity)
import           Text.Casing

data DeployEnv = Development | Staging | Production
  deriving (Show, Generic, Eq)
instance FromJSON DeployEnv

data Config = Config
  { apiPort           :: Int
  , postgresUser      :: String
  , postgresPassword  :: String
  , postgresHost      :: String
  , postgresDatabase  :: String
  , postgresPort      :: String
  , elasticsearchUrl  :: String
  , deployEnv         :: DeployEnv -- ^ 環境(例: Development, Staging, Production)
  , deployCorsOrigins :: [String]  -- ^ CORS Originに指定したいもの
  , dbPoolStripeNum   :: Int
  , dbPoolResourceNum :: Int
  , dbPoolKeepTime    :: Double
  , logLevel          :: Severity
  , apiSecretKey      :: String
  } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = quietSnake}

loadConfig :: IO Config
loadConfig = C.loadYamlSettings ["config.yaml"] [] C.useEnv