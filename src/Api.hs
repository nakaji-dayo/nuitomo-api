{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Api where

import           App
import           Auth
import qualified Config
import           Handler.Error
import           Handler.Post
import           Handler.System
import           Handler.User
import           Katip
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant                     hiding (Context)
import           Servant.Auth.Server
import           Servant.Swagger.Tags
import           Type

import           Debug.Trace
-- instance (HasServer (MultipartForm a b :> api) context, HasMock api context) => HasMock (MultipartForm a b :> api) context where
--   mock _ ctx _ = mock (Proxy :: Proxy api) ctx

-- instance HasSwagger api => HasSwagger (MultipartForm a b :> api) where
--   toSwagger Proxy = toSwagger $ Proxy @api

type Protected =
  Tags "Post" :> PostAPI
  :<|> Tags "User" :> UserAPI

type PostAPI =
  Summary "List posts" :> "posts" :> Get '[JSON] [PostResponse]
  :<|> Summary "Create Post" :> "posts" :> ReqBody '[JSON] CreatePostRequest :> Post '[JSON] ResourceId

type UserAPI =
  Summary "List User" :> "users" :> Get '[JSON] [UserResponse]
  :<|> Summary "Create User" :> "users" :> ReqBody '[JSON] CreateUserRequest :> Post '[JSON] ResourceId

type UnProtected =
  Tags "System" :>
  ( Summary "version" :> "_version" :> Get '[JSON] String
  )

type API auths = Auth auths AuthUser :> Protected
  :<|> UnProtected

protected :: AuthResult AuthUser -> ServerT Protected AppM
protected (Authenticated au) =
  postApi au
  :<|> userApi au
protected x                  = trace (show x) $ throwAll err401

postApi :: AuthUser -> ServerT PostAPI AppM
postApi au = getPostsR au :<|> postPostsR au

userApi :: AuthUser -> ServerT UserAPI AppM
userApi au = getUsersR au :<|> postUsersR au

unprotected :: ServerT UnProtected AppM
unprotected =
  systemAPI
  where
    systemAPI = getVersionR

server :: ServerT (API auths) AppM
server = protected :<|> unprotected

-- server' = protected Servant.Auth.Server.NoSuchUser :<|> unprotected

-- app :: Context -> LogEnv -> Application
app :: Context -> JWTSettings -> LogEnv -> Application
app ctx@Context{config=cfg} jwtCfg logEnv =
  let
    setting = defaultCookieSettings :. jwtCfg :. EmptyContext
    corigin = case Config.deployEnv cfg of
      Config.Development -> Nothing
      _                  -> Nothing
      -- _ -> Just (BSC.pack <$> Config.deployCorsOrigins config, False)
    cors' = cors (const $ Just simpleCorsResourcePolicy
                   { corsMethods = ["GET", "HEAD", "POST", "PUT", "DELETE", "OPTIONS"]
                   , corsRequestHeaders =   [ "Accept", "Accept-Language", "Content-Language", "Content-Type", "Authorization"]
                   , corsOrigins = corigin
                   })
    psetting = Proxy :: Proxy '[CookieSettings, JWTSettings]
    hserver = hoistServerWithContext api psetting (nt ctx (logEnv, "serve") . handleGeneralException) server :: Server (API auths)
  in
    cors' $ serveWithContext api setting hserver

api :: Proxy (API '[JWT])
api = Proxy

serve :: IO ()
serve = do
  (ctx, logEnv) <- initialize
  let jwtCfg = mkJwtCfg (Config.apiSecretKey $ config ctx)
  jwtCfg' <- getFirebaseAuthJwtSettings (signingKey jwtCfg)
  run (Config.apiPort (config ctx)) (app ctx jwtCfg' logEnv)
