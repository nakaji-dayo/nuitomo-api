{-# LANGUAGE DuplicateRecordFields #-}
module Service.Notification where

import           App
import           Entity
import           Entity.Notification
import           Service.Util
import           Type

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
