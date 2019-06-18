{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Auth where

import           Control.Lens
import           Control.Monad.Except
import           Crypto.JOSE.Error          (Error)
import           Crypto.JOSE.JWA.JWS
import           Crypto.JOSE.JWK
import           Crypto.JOSE.Types
import           Crypto.JWT
import           Data.Aeson
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.PEM                   as PEM
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.X509
import           Debug.Trace
import           Debug.Trace
import           GHC.Generics
import           Network.HTTP.Simple
import           Servant.Auth.Server
import           Type

type AccountId = String

data AuthUser = AuthUser
  {
    sub :: AccountId
  } deriving (Show, Generic)
instance ToJSON AuthUser where
  toJSON = genericToJSON customOptions
instance FromJSON AuthUser where
  parseJSON = genericParseJSON customOptions
instance FromJWT AuthUser where -- todo: validate iss
  decodeJWT cs = trace (show ("cs", cs)) $ case cs ^? claimSub . _Just . string of
    Just x  -> Right $ AuthUser $ T.unpack x
    Nothing -> Left "sub not found"
instance ToJWT AuthUser

testUser :: String -> AuthUser
testUser = AuthUser

mkJwtCfg :: String -> JWTSettings
mkJwtCfg k = defaultJWTSettings myKey
  where myKey = fromKeyMaterial
          $ OctKeyMaterial (OctKeyParameters (Base64Octets $ BSC.pack k))

generateToken
  :: JWTSettings
     -> String
     -> IO (Either Error LBSC.ByteString)
generateToken jwtCfg uid = makeJWT (AuthUser uid) jwtCfg Nothing

printNewKey :: IO ()
printNewKey = do
  k <- generateKey
  case k ^. jwkMaterial of
    OctKeyMaterial (OctKeyParameters b) ->
      LBSC.putStrLn $ encode b
    _ -> error "unsupported material"

getFirebaseAuthJwtSettings :: JWK -> IO JWTSettings
getFirebaseAuthJwtSettings skey = do
  cert <- getCert fkey1
  cert2 <- getCert fkey2
  jwks <- forM [cert, cert2] $ \x -> runExceptT (fromX509Certificate x) >>= \case
    Left (e :: JWTError) -> error $ show e
    Right jwk -> pure jwk
  print jwks
  return $ JWTSettings skey (Just RS256) (JWKSet jwks) (\x -> trace (show x) Matches) -- todo validate aud
  where
    getCert fk = do
       --res <- httpBS "https://www.googleapis.com/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com"

      pem <- case PEM.pemParseBS (T.encodeUtf8 fk) of
        Left e    -> error $ show e
        Right [e] -> pure e
        Right xs  -> error $ show ("right xs", xs)
      let cert = case decodeSignedCertificate (PEM.pemContent pem) of
            Left e  -> error $ show e
            Right x -> x
      print cert
      return cert


fkey1 = "-----BEGIN CERTIFICATE-----\nMIIDHDCCAgSgAwIBAgIIbXbaIQZL78cwDQYJKoZIhvcNAQEFBQAwMTEvMC0GA1UE\nAxMmc2VjdXJldG9rZW4uc3lzdGVtLmdzZXJ2aWNlYWNjb3VudC5jb20wHhcNMTkw\nNjA1MjEyMDU1WhcNMTkwNjIyMDkzNTU1WjAxMS8wLQYDVQQDEyZzZWN1cmV0b2tl\nbi5zeXN0ZW0uZ3NlcnZpY2VhY2NvdW50LmNvbTCCASIwDQYJKoZIhvcNAQEBBQAD\nggEPADCCAQoCggEBAMFQMriyb7HnFGXih8MyAa3sW1CiT9nY4kdOfuifV8WGv6xr\nyxlwwQfeRBG52nzGOdeGu4rzS7L3Ckk6NYV9lWieDY9chT0ZJ84PWhCZZMNcJ6ol\nzc9e0K0HJJC+vt3zNxBzrVRELYItjhkwZOPLTGmPUAzq/w1wJpBDm664OWVA9fKp\n69v6XhAB/V9erBGNlKF6VRPpv9JbKA2SrXJGiAOMUemxHhCdI2l7jH9wgh51S4oI\neFZ5smYkjF/a+ec3T1PaTBC4Kn1/+vfbNmDcVxYfgdfczYfmif39tLujFO7Y1b6J\nWQvEHsp6f59A/uTe4o9dskipPGQEIpOXe6hwW5sCAwEAAaM4MDYwDAYDVR0TAQH/\nBAIwADAOBgNVHQ8BAf8EBAMCB4AwFgYDVR0lAQH/BAwwCgYIKwYBBQUHAwIwDQYJ\nKoZIhvcNAQEFBQADggEBAHlXlTv+YjJnIkOXb9SNMRO9ZSeFAV0ld2ETg9B8FYsY\nZ5L/AnkOxLzYa2Q305Oi6pg4UDg2iLBXK7EFVSileC9DQwISoS/GffrOOtWxs48o\nUoiNd4eAbswxuXIjGoq5We9JT9hYxSVubsPYys1pcjQCX+NttehZpnaJ2yam8gLV\n3+2NenC1PUj6DBvycFvs4QuHNVBJfImhp2sjV/yw/DNWSLXqWCImMxLJCQLOAzYo\nXpkfOK+IBG4P3WHwLty1ZtwuIr+475WIvT5iyZdRmg8doKx4qF7ILYmvtBUipoY6\np7bcc0tn/qr11UKA6xJn+tJ/xNXaEBXcrfozhFIG71I=\n-----END CERTIFICATE-----\n"

-- todo: key頻繁に変わるかも？
-- fkey2 = "-----BEGIN CERTIFICATE-----\nMIIDHDCCAgSgAwIBAgIIHsFXBrzdL0YwDQYJKoZIhvcNAQEFBQAwMTEvMC0GA1UE\nAxMmc2VjdXJldG9rZW4uc3lzdGVtLmdzZXJ2aWNlYWNjb3VudC5jb20wHhcNMTkw\nNTI4MjEyMDU1WhcNMTkwNjE0MDkzNTU1WjAxMS8wLQYDVQQDEyZzZWN1cmV0b2tl\nbi5zeXN0ZW0uZ3NlcnZpY2VhY2NvdW50LmNvbTCCASIwDQYJKoZIhvcNAQEBBQAD\nggEPADCCAQoCggEBAK9Lsu7vi+NhpGXViMiQPvYNrtCXf4MXhEEyJO123o+90VC9\njJGq7OOWLnB/JAjOqxY8G0rWn6Y4tBimcTvHK5+yCIeCZNDShkYAjQhS41cgYTW3\nE/UaQ0wpPlHjgKtT7bnKqWfBi9NL0I7GM9sgPyJ4BcDGajXbFedCFdCfXWl4qO1i\nq1lRvZVQX+79HOHL1/PDdfKlKX8DfBVTtupmVTSyEGpbr7zdMk3Smz3zLOiBXmsN\nMrdF47P9wx6G9LtgLwN52lG+AakadrW0ewFrGryr8Pybu12EUNmt0/cBLVLAw/GD\np5x0oSRRSKsLLEA4U208gr6u7csJ9MOAg09zrP8CAwEAAaM4MDYwDAYDVR0TAQH/\nBAIwADAOBgNVHQ8BAf8EBAMCB4AwFgYDVR0lAQH/BAwwCgYIKwYBBQUHAwIwDQYJ\nKoZIhvcNAQEFBQADggEBAG5mvRvd5ec6P3+kURgqXAy3NW5wPiHRiAPPzz41CBom\nFz/OCjXZ2bfUSVB4DG5WXdOPCbuUG6UFxPSNhqvUNfXv6j9p9QRJjjwRwACfod+s\nddz9lIQT/JCgO7hx8ILhhwSAmFtl75EWjDfn4Srvi8+nh1oW4v1XmVDSpJBs/y/k\nVCqjpONpChCrnDoJH5A2qK1MsbVCjSwyM998uXDTMDSSFuhVEdIvk0RZ5BpgZnGX\ntku/DINys3qy93fApXBHZf9ZE5RSH+Egvv6IV7r2xKv6q94dO4gQGsNkVMSeijgm\nMzDbyPPHQ7jfqRJ1wlkOOJW/PMe/AupHBn1/ycxcTa0=\n-----END CERTIFICATE-----\n"
fkey2 = "-----BEGIN CERTIFICATE-----\nMIIDHDCCAgSgAwIBAgIIdjVTTMeinDQwDQYJKoZIhvcNAQEFBQAwMTEvMC0GA1UE\nAxMmc2VjdXJldG9rZW4uc3lzdGVtLmdzZXJ2aWNlYWNjb3VudC5jb20wHhcNMTkw\nNjEzMjEyMDU1WhcNMTkwNjMwMDkzNTU1WjAxMS8wLQYDVQQDEyZzZWN1cmV0b2tl\nbi5zeXN0ZW0uZ3NlcnZpY2VhY2NvdW50LmNvbTCCASIwDQYJKoZIhvcNAQEBBQAD\nggEPADCCAQoCggEBAMLJICckCQaObIgcY6Yc8f4jaQOv6jGziQeMuhtzJWpTjuQX\nrQ+s9ZS73oum3MbsFCozRIbfqd6q7TwAqY4umuxBG6m8Vih4SC39TGP8HYPMbk3y\neD9Z9MnQHrn0B63N0rgg3K5aQVf73TSFUCG9TCSxSQgKA3MhlZ0St6Co4rj7PnmS\nLduEAK+cd/gXSCpe7DJv4gJ81DgeD94abEDts2ooqKe9PnP2kUck2AMbIxgsxVih\nVkstNRKrVFMIHxvDFgiUqa/b/gQwk2FlCS2EXNgcoTzDLtzKGbdkXFP84U0f95Ty\nLynf3pL25tumgjVRxPOy1BNop3eaqMiZtYaix/sCAwEAAaM4MDYwDAYDVR0TAQH/\nBAIwADAOBgNVHQ8BAf8EBAMCB4AwFgYDVR0lAQH/BAwwCgYIKwYBBQUHAwIwDQYJ\nKoZIhvcNAQEFBQADggEBAD8RWbeJ8QMA1NYpvxvtJ8sjANYWy4pQ2UffeuHwUkEU\n4bgbtNMB6CTf/RuNYfmS2LEmM6V0v6CGEZ2wb4pUjiKw8mqaQPfq9/mJe3jTx3RY\niibFiVp+nP8fYT22G/VD3VblSJS130N4SEM70q31NBTvZ3ASBENGkHddDOpAkQ0Y\n9EZtjj+ap9Fuqdt7xC5mGXCZHTw0k0z1SDo6+06VEk+SqGhEHeuMx8o949OR4GMG\npiMpmzS4yzAAurAQLEf732f/j5H4TnSAtVYEyLQwkHlDpG8HxALVwzDi0pJ1Zi+9\n3OHhDZNrslhtA4sdxLaAXQl45B/z2p/GOvpV/cGb41E=\n-----END CERTIFICATE-----\n"
