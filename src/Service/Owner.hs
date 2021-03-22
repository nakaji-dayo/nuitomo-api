{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
module Service.Owner where

import           App
import           Control.Monad
import           Data.ByteString.Base58
import           Data.ByteString.Char8      as BS
import           Data.ByteString.Random.MWC
import           Entity
import           Entity.OwnerKey
import           Entity.OwnerToken
import           Entity.OwnerUser
import           EntityId
import           Query                      as Q
import           Service.Exception
import           Service.Util
import           Type

getOwnerKey :: MonadService m => String -> m String
getOwnerKey aid = do
  o <- selectOneM Q.selectOwnerKey aid
  case o of
    Just k -> pure $ k ^. #key
    Nothing -> do
      k <- liftIO genKey
      tid <- OwnerKeyId <$> getTid
      let o' = OwnerKey
            { id = tid
            , ownerId = aid
            , key = k
            }
      insertM insertOwnerKey o'
      pure k
  where
    genKey =
      BS.unpack . encodeBase58 flickrAlphabet <$> random 5

checkPrimaryOwner ::
  MonadService m => String -> UserId -> m OwnerUserId
checkPrimaryOwner = curry (getResource selectPrimaryOwner)

getOwners :: MonadService m => String -> UserId -> m [(OwnerKey, OwnerUser)]
getOwners aid uid = checkPrimaryOwner aid uid >> queryM selectOwners uid

addOwner :: MonadService m => String -> UserId -> String -> m ()
addOwner aid uid key = do
  liftIO $ print (aid, uid, key)
  checkPrimaryOwner aid uid
  newOwner <- getResource selectOwnerByKey key
  when (newOwner == aid) $ throwM (OtherException "cant add self")
  oid <- OwnerUserId <$> getTid
  let ou = OwnerUser
        { id = oid
        , userId = uid
        , ownerId = newOwner
        , isPrimary = False
        }
  insertM insertOwnerUser ou
  pure ()

deleteOwner ::
  MonadService m => String -> OwnerUserId -> m ()
deleteOwner aid ouid = do
  ou <- getResource selectOwnerUser ouid
  checkPrimaryOwner aid (ou ^. #userId)
  deleteM deleteOwnerUser ouid
  pure ()

setOwnerPushToken aid token = runTransactionM $ do
  selectOneM Q.selectOwnerToken token
    >>= \case
    Just ot -> do
      let ot' = ot & #ownerId .~ aid
      keyUpdateM updateOwnerToken ot'
      pure ot'
    Nothing -> do
      tid <- OwnerTokenId <$> getTid
      let ot = OwnerToken
            { id = tid
            , ownerId = aid
            , token = token
            }
      insertM insertOwnerToken ot
      pure ot
