{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
module Service.Notification where

import           App
import           Auth
import           Control.Concurrent
import           Entity
import           Entity.Notification
import           Entity.User         as E
import           Network.Expo.Client as EX
import           Query
import           Service.Util
import           Type

getNotifications :: MonadService m => AccountId -> Maybe ResourceId -> m [(Notification, User, Maybe User, Maybe Post)]
getNotifications (AccountId uid) mcursor = queryM (selectNotifications mcursor) uid

createNotification
     :: (MonadService m) =>
     ResourceId
     -> NotificationType -> Maybe ResourceId -> Maybe ResourceId -> m Integer
createNotification uid ntype mruid mrpid = do
  tid <- getTid
  now <- getCurrentLocalTime
  let n = Notification
        { id = tid
        , userId = uid
        , notificationType = fromEnum' ntype
        , refUserId = mruid
        , refPostId = mrpid
        , createdAt = now
        }
  sendPushNotification uid ntype
  insertM insertNotification n

sendPushNotification :: MonadService m => ResourceId -> NotificationType -> m ()
sendPushNotification uid ty = do
  u <- getResource E.selectUser uid
  ts <- queryM selectTokensByUserId uid
  let b = pushBody ty (u ^. #name)
  let mkReq :: OwnerToken -> SendRequest
      mkReq t = sendRequest (t ^. #token)
        & #body ?~ b
        & #sound ?~ "default"
        & #badge ?~ 1
        & #_displayInForeground ?~ True
  liftIO $ forkIO $ do
    r <- batchSend (fmap mkReq ts)
    print r
    pure ()
  pure ()


pushBody NotifyReply n   = n <> "に返信が届いています"
pushBody NotifyFollow n  = n <> "をフォローしたユーザーがいます"
pushBody NotifyLike n    = n <> "の投稿が「いいね」されました"
pushBody NotifyMention n = n <> "にロボットから質問が届いています"
