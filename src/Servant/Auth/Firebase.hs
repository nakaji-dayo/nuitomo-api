-- fork from: https://github.com/deckgo/deckdeckgo/blob/4c61f0f366e15cf5097bb947294d519473d92b85/infra/firebase-login/src/Servant/Auth/Firebase.hs#L53
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Servant.Auth.Firebase where

import           Control.Lens                               hiding ((.=))
import           Control.Monad
import           Control.Monad.Except
import qualified Crypto.JOSE.JWK                            as JWK
import qualified Crypto.JWT                                 as JWT
import qualified Data.Aeson                                 as Aeson
import qualified Data.ByteString                            as BS
import qualified Data.ByteString.Lazy                       as BL
import qualified Data.HashMap.Strict                        as HMS
import qualified Data.PEM                                   as PEM
import           Data.Proxy
import qualified Data.Text                                  as T
import qualified Data.Text.Encoding                         as T
import           Data.Word8                                 (isSpace, toLower)
import qualified Data.X509                                  as X509
import qualified Network.HTTP.Client                        as HTTP
import qualified Network.HTTP.Simple                        as HTTP
import qualified Network.URI                                as URI
import qualified Network.Wai                                as Wai
import qualified Servant
import           Servant.API
import qualified Servant.Server.Internal.RoutingApplication as Servant
import qualified Servant.Swagger                            as Servant

data Protected

newtype ProjectId = ProjectId { unFirebaseProjectId :: T.Text }

data FirebaseLoginSettings = FirebaseLoginSettings
  { firebaseLoginProjectId :: ProjectId
  , firebaseLoginGetKeys   :: IO (HMS.HashMap T.Text T.Text)
  }

defaultFirebaseLoginSettings
  :: HTTP.Manager -> ProjectId -> FirebaseLoginSettings
defaultFirebaseLoginSettings mgr pid = FirebaseLoginSettings
    { firebaseLoginProjectId = pid
    , firebaseLoginGetKeys = HTTP.getResponseBody <$> HTTP.httpJSON req
    }
  where
    -- TODO: proper error handling here
    req =
      HTTP.setRequestSecure True .
      HTTP.setRequestPort 443 .
      HTTP.setRequestHost "www.googleapis.com" .
      HTTP.setRequestPath "/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com" .
      HTTP.setRequestManager mgr $
      HTTP.defaultRequest {
          HTTP.responseTimeout = HTTP.responseTimeoutMicro (5 * 1000 * 1000)
        }

newtype AccountId = AccountId { unAccountId :: String }
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Show, Eq)

newtype UnverifiedJWT = UnverifiedJWT JWT.SignedJWT

-- TODO: MAKE SURE PATTERN MATCH FAILURES AREN'T PROPAGATED TO CLIENT!!!
verifyUser :: FirebaseLoginSettings -> UnverifiedJWT -> IO AccountId
verifyUser settings (UnverifiedJWT jwt) = do
  jwkmap <- firebaseLoginGetKeys settings
  let projectId = unFirebaseProjectId $ firebaseLoginProjectId settings

  t <- case jwt ^.. JWT.signatures . JWT.header . JWT.kid of
    [Just (JWT.HeaderParam () t)] -> pure t
    xs -> error $ "Expected exactly one signature with 'kid', got: " <> show xs

  jwkct <- case HMS.lookup t jwkmap of
    Nothing -> error $ "Could not find key " <> show t <> " in response"
    Just ct -> pure ct

  -- TODO: get rid of 'error'
  pem <- case PEM.pemParseBS (T.encodeUtf8 jwkct) of
    Left e    -> error $ show e
    Right [e] -> pure e
    Right xs  -> error $ show xs

  cert <- case X509.decodeSignedCertificate (PEM.pemContent pem) of
    Left e  -> error $ show e
    Right c -> pure c

  jwk <- runExceptT (JWK.fromX509Certificate cert) >>= \case
    Left (e :: JWT.JWTError) -> error $ show e
    Right jwk -> pure jwk

  issUri <- case URI.parseURI $ "https://securetoken.google.com/" <> T.unpack projectId of
    Just issUri -> pure issUri
    Nothing     -> error "Could not use project ID in URI"

  let config =
        JWT.defaultJWTValidationSettings
          (\sou -> Just projectId == sou ^? JWT.string) & -- aud
          JWT.issuerPredicate .~ (\sou -> Just issUri ==  sou ^? JWT.uri) & -- iss
          JWT.allowedSkew .~ 60 * 5
  runExceptT (JWT.verifyClaims config jwk jwt) >>= \case
    Right cs ->
      case cs ^. JWT.claimSub of
        Nothing -> error "Could not get a subject from claim set"
        Just sou -> case sou ^? JWT.string of
          Nothing -> error "Expected subject to be string"
          Just u  -> pure (AccountId $ T.unpack u)
    Left (e :: JWT.JWTError) -> error (show e)

instance FromHttpApiData UnverifiedJWT where
  parseUrlPiece = const $ Left "No support for JWT"
  parseHeader bs = case JWT.decodeCompact (BL.fromStrict bs) of
    Left (e :: JWT.Error) -> Left $ T.pack $ show e
    Right jwt             -> Right $ UnverifiedJWT jwt

-- instance
--     ( Servant.HasClient m sub
--     , Servant.RunClient m ) => Servant.HasClient m (Protected :> sub) where
--   -- TODO: something better than just Text
--   type Client m (Protected :> sub) = T.Text -> Servant.Client m sub
--   clientWithRoute p1 Proxy req = \bs ->
--     Servant.clientWithRoute
--       p1 (Proxy :: Proxy sub)
--       (Servant.Client.addHeader "Authorization" ("Bearer " <> bs) req)
--   hoistClientMonad p1 Proxy hoist c = \bs ->
--     Servant.Client.hoistClientMonad p1 (Proxy :: Proxy sub) hoist (c bs)

-- | Find and decode an 'Authorization' header from the request as JWT
decodeJWTHdr :: Wai.Request -> Either String UnverifiedJWT
decodeJWTHdr req = do
    ah <- case lookup "Authorization" (Wai.requestHeaders req) of
      Just x  -> Right x
      Nothing -> Left "No authorization header"
    let (b, rest) = BS.break isSpace ah
    guard (BS.map toLower b == "bearer")
    tok <- case snd <$> BS.uncons rest of
      Nothing -> Left "No token"
      Just x  -> Right x
    case JWT.decodeCompact (BL.fromStrict tok) of
      Left (e :: JWT.Error) -> Left $ show e <> ": " <> show rest
      Right jwt             -> Right (UnverifiedJWT jwt)

runJWTAuth :: FirebaseLoginSettings -> Wai.Request -> Servant.DelayedIO AccountId
runJWTAuth settings req = case decodeJWTHdr req of
    Left e     -> error $ "bad auth: " <> e -- TODO: delayedFailFatal
    Right ujwt -> liftIO $ verifyUser settings ujwt

instance
    ( Servant.HasContextEntry context FirebaseLoginSettings
    , Servant.HasServer sub context
    ) => Servant.HasServer (Protected :> sub) context where
  type ServerT (Protected :> sub) m = AccountId -> Servant.ServerT sub m

  route Proxy c subserver =
      Servant.route (Proxy :: Proxy sub)
        c (subserver `Servant.addAuthCheck` authCheck)
    where
      authCheck :: Servant.DelayedIO AccountId
      authCheck = Servant.withRequest $ runJWTAuth (Servant.getContextEntry c)

  hoistServerWithContext Proxy p hoist s uid =
    Servant.hoistServerWithContext (Proxy :: Proxy sub) p hoist (s uid)

instance Servant.HasSwagger sub => Servant.HasSwagger (Protected :> sub) where
  toSwagger Proxy = Servant.toSwagger (Proxy :: Proxy sub)
