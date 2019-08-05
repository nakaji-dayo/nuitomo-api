{-# LANGUAGE DuplicateRecordFields #-}
module Service.Notification where

import           App
import           Auth
import           Entity
import           Entity.Notification
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
  insertM insertNotification n
