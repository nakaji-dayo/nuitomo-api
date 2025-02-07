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
import           EntityId
import           Handler.Error
import           Handler.Post
import           Handler.System
import           Handler.User
import           Katip
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant                     hiding (Context)
import           Servant.Auth.Firebase
import           Servant.Swagger.Tags
import           Type

import           Debug.Trace
-- instance (HasServer (MultipartForm a b :> api) context, HasMock api context) => HasMock (MultipartForm a b :> api) context where
--   mock _ ctx _ = mock (Proxy :: Proxy api) ctx

-- instance HasSwagger api => HasSwagger (MultipartForm a b :> api) where
--   toSwagger Proxy = toSwagger $ Proxy @api

type PostAPI =
  Summary "List posts" :> "posts" :> QueryParam "uid" UserId :> QueryParam "cursor" PostId :> QueryParam "scope" String :> Get '[JSON] [PostResponse]
  :<|> Summary "detail posts" :> "posts" :> Capture "id" PostId :> Get '[JSON] PostResponse
  :<|> Summary "Create Post" :> "posts" :> ReqBody '[JSON] CreatePostRequest :> Post '[JSON] PostId
  :<|> "likes" :> ReqBody '[JSON] CreateLikeRequest :> Post '[JSON] LikeId
  :<|> "likes" :> ReqBody '[JSON] CreateLikeRequest :> Delete '[JSON] ()

postApi :: AccountId -> ServerT PostAPI AppM
postApi au = getPostsR au
  :<|> getPostR au
  :<|> postPostsR au
  :<|> postLikesR au
  :<|> deleteLikesR au

type UserAPI =
  "users" :> (
  Summary "List User" :> Get '[JSON] [UserResponse]
  :<|> Summary "Search User" :> "_search" :> QueryParam "q" String :> Get '[JSON] [UserResponse]
  :<|> Summary "User" :> Capture "id" UserId :> Get '[JSON] DetailUserResponse
  :<|> Summary "Update User" :> Capture "id" UserId :> ReqBody '[JSON] UpdateUserRequest :> Patch '[JSON] ()
  :<|> Summary "Create User" :> ReqBody '[JSON] CreateUserRequest :> Post '[JSON] UserId
  :<|> Capture "id" UserId :> "follows" :> Summary "Create Follow" :> ReqBody '[JSON] CreateFollowRequest :> Post '[JSON] ()
  :<|> Capture "id" UserId :> "follows" :> Summary "Delete Follow" :> ReqBody '[JSON] CreateFollowRequest :> Delete '[JSON] ()
  :<|> Capture "id" UserId :> "followees" :> Summary "List Followee" :> Get '[JSON] [UserResponse]
  :<|> Capture "id" UserId :> "followers" :> Summary "List Follower" :> QueryParam "scope" String :> Get '[JSON] [UserResponse]
  :<|> Capture "id" UserId :> "owners" :> Get '[JSON] [OwnerResponse]
  :<|> Capture "id" UserId :> "owners" :> ReqBody '[JSON] String :> Post '[JSON] ()
  )


userApi :: AccountId -> ServerT UserAPI AppM
userApi au = (
  getUsersR au
  :<|> getUsersSearchR au
  :<|> getUserR au
  :<|> patchUserR au
  :<|> postUsersR au
  --
  :<|> postFollowsR au
  :<|> deleteFollowsR au
  :<|> getFolloweesR au
  :<|> getFollowersR au
  --
  :<|> getOwnersR au
  :<|> postOwnersR au
  )

type OtherAPI =
  "notifications" :> QueryParam "cursor" NotificationId :> Get '[JSON] [GetNotification]
  :<|> "me" :> Get '[JSON] MeResponse
  :<|> "owners"  :> Capture "id" OwnerUserId :> Delete '[JSON] ()
  :<|> "push_tokens" :> ReqBody '[JSON] String :> Post '[JSON] ()

otherApi au =
  getNotificationsR au
  :<|> getMeR au
  :<|> deleteOwnersR au
  :<|> postPushTokensR au

type UnProtected =
  Tags "System" :>
  ( Summary "version" :> "_version" :> Get '[JSON] String
    :<|> "_twitter_redirect" :> QueryParam "link_to" String :> QueryParam "oauth_token" String :> QueryParam "oauth_verifier" String :> Get '[JSON] ()
  )

unprotected :: ServerT UnProtected AppM
unprotected =
  systemAPI
  where
    systemAPI = getVersionR :<|> getTwitterRedirectR

type API = Protected :> Protected'
  :<|> UnProtected

type Protected' =
  Tags "Post" :> PostAPI
  :<|> Tags "User" :> UserAPI
  :<|> Tags "Other" :> OtherAPI

protected :: AccountId -> ServerT Protected' AppM
protected i =
  postApi i
  :<|> userApi i
  :<|> otherApi i

server :: ServerT API AppM
server = protected :<|> unprotected

-- server' = protected Servant.Auth.Server.NoSuchUser :<|> unprotected

-- app :: Context -> LogEnv -> Application
app :: Context -> FirebaseLoginSettings -> LogEnv -> Application
app ctx@Context{config=cfg} settings logEnv =
  let
    corigin = case Config.deployEnv cfg of
      Config.Development -> Nothing
      _                  -> Nothing
      -- _ -> Just (BSC.pack <$> Config.deployCorsOrigins config, False)
    cors' = cors (const $ Just simpleCorsResourcePolicy
                   { corsMethods = ["GET", "HEAD", "POST", "PUT", "DELETE", "OPTIONS"]
                   , corsRequestHeaders =   [ "Accept", "Accept-Language", "Content-Language", "Content-Type", "Authorization"]
                   , corsOrigins = corigin
                   })
    psetting = Proxy :: Proxy '[FirebaseLoginSettings]
    s = settings :. EmptyContext
    hserver = hoistServerWithContext api psetting (nt ctx (logEnv, "serve") . handleGeneralException) server :: Server API
  in
    cors' $ serveWithContext api s hserver

api :: Proxy API
api = Proxy

serve :: IO ()
serve = do
  (ctx, logEnv) <- initialize
  let s = defaultFirebaseLoginSettings (httpClientManager ctx) (ProjectId "nuitomo-c4587")
  run (Config.apiPort (config ctx)) (app ctx s logEnv)
