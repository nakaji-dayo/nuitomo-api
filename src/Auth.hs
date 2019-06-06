{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Auth where

import           Control.Lens
import           Crypto.JOSE.Error          (Error)
import           Crypto.JOSE.JWK
import           Crypto.JOSE.Types
import           Data.Aeson
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           GHC.Generics
import           Servant.Auth.Server
import           Type

data AuthType = User | Guest
  deriving (Show, Generic, Read, Enum)

$(deriveApiFieldSumType ''AuthType 'Auth.User)

data AuthUser = AuthUser
  {
    _id :: ResourceId
  } deriving (Show, Generic)
instance ToJSON AuthUser where
  toJSON = genericToJSON customOptions
instance FromJSON AuthUser where
  parseJSON = genericParseJSON customOptions
makeFieldsNoPrefix ''AuthUser
instance FromJWT AuthUser
instance ToJWT AuthUser

testUser :: ResourceId -> AuthUser
testUser = AuthUser

mkJwtCfg :: String -> JWTSettings
mkJwtCfg k = defaultJWTSettings myKey
  where myKey = fromKeyMaterial
          $ OctKeyMaterial (OctKeyParameters (Base64Octets $ BSC.pack k))

generateToken
  :: JWTSettings
     -> ResourceId
     -> IO (Either Error LBSC.ByteString)
generateToken jwtCfg uid = makeJWT (AuthUser uid) jwtCfg Nothing

printNewKey :: IO ()
printNewKey = do
  k <- generateKey
  case k ^. jwkMaterial of
    OctKeyMaterial (OctKeyParameters b) ->
      LBSC.putStrLn $ encode b
    _ -> error "unsupported material"