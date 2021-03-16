{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module EntityId where

import           Data.Aeson
import           Data.Default.Class
import           Data.Swagger                         (ToSchema(..))
import           Data.Swagger.ParamSchema             (ToParamSchema)
import           Database.HDBC                        (SqlValue)
import           Database.HDBC.Schema.Driver          ()
import           Database.HDBC.Schema.PostgreSQL      ()
import           Database.Record
import           Database.Record.Persistable          ()
import           Database.Relational.ProjectableClass
import           GHC.Generics
import           GHC.Int
import           Test.QuickCheck
import           Language.Haskell.TH             (TypeQ)
import           Servant.API
import           Data.Proxy
newtype FollowId = FollowId { unFollowId :: Int64 }
  deriving (Generic, Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Default, Arbitrary, PersistableWidth
           , LiteralSQL, FromSql SqlValue, ToSql SqlValue, FromHttpApiData, Num
           , HasColumnConstraint NotNull)
instance ToSchema FollowId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)
instance ToParamSchema FollowId

newtype LikeId = LikeId { unLikeId :: Int64 }
  deriving (Generic, Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Default, Arbitrary, PersistableWidth
           , LiteralSQL, FromSql SqlValue, ToSql SqlValue, FromHttpApiData, Num
           , HasColumnConstraint NotNull)
instance ToSchema LikeId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)
instance ToParamSchema LikeId

newtype NotificationId = NotificationId { unNotificationId :: Int64 }
  deriving (Generic, Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Default, Arbitrary, PersistableWidth
           , LiteralSQL, FromSql SqlValue, ToSql SqlValue, FromHttpApiData, Num
           , HasColumnConstraint NotNull)
instance ToSchema NotificationId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)
instance ToParamSchema NotificationId

newtype OwnerKeyId = OwnerKeyId { unOwnerKeyId :: Int64 }
  deriving (Generic, Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Default, Arbitrary, PersistableWidth
           , LiteralSQL, FromSql SqlValue, ToSql SqlValue, FromHttpApiData, Num
           , HasColumnConstraint NotNull)
instance ToSchema OwnerKeyId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)
instance ToParamSchema OwnerKeyId

newtype OwnerTokenId = OwnerTokenId { unOwnerTokenId :: Int64 }
  deriving (Generic, Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Default, Arbitrary, PersistableWidth
           , LiteralSQL, FromSql SqlValue, ToSql SqlValue, FromHttpApiData, Num
           , HasColumnConstraint NotNull)
instance ToSchema OwnerTokenId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)
instance ToParamSchema OwnerTokenId

newtype OwnerUserId = OwnerUserId { unOwnerUserId :: Int64 }
  deriving (Generic, Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Default, Arbitrary, PersistableWidth
           , LiteralSQL, FromSql SqlValue, ToSql SqlValue, FromHttpApiData, Num
           , HasColumnConstraint NotNull)
instance ToSchema OwnerUserId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)
instance ToParamSchema OwnerUserId

newtype PostId = PostId { unPostId :: Int64 }
  deriving (Generic, Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Default, Arbitrary, PersistableWidth
           , LiteralSQL, FromSql SqlValue, ToSql SqlValue, FromHttpApiData, Num
           , HasColumnConstraint NotNull)
instance ToSchema PostId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)
instance ToParamSchema PostId

newtype PostImageId = PostImageId { unPostImageId :: Int64 }
  deriving (Generic, Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Default, Arbitrary, PersistableWidth
           , LiteralSQL, FromSql SqlValue, ToSql SqlValue, FromHttpApiData, Num
           , HasColumnConstraint NotNull)
instance ToSchema PostImageId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)
instance ToParamSchema PostImageId

newtype QuestionId = QuestionId { unQuestionId :: Int64 }
  deriving (Generic, Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Default, Arbitrary, PersistableWidth
           , LiteralSQL, FromSql SqlValue, ToSql SqlValue, FromHttpApiData, Num
           , HasColumnConstraint NotNull)
instance ToSchema QuestionId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)
instance ToParamSchema QuestionId

newtype QuestionUserId = QuestionUserId { unQuestionUserId :: Int64 }
  deriving (Generic, Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Default, Arbitrary, PersistableWidth
           , LiteralSQL, FromSql SqlValue, ToSql SqlValue, FromHttpApiData, Num
           , HasColumnConstraint NotNull)
instance ToSchema QuestionUserId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)
instance ToParamSchema QuestionUserId

newtype ReportId = ReportId { unReportId :: Int64 }
  deriving (Generic, Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Default, Arbitrary, PersistableWidth
           , LiteralSQL, FromSql SqlValue, ToSql SqlValue, FromHttpApiData, Num
           , HasColumnConstraint NotNull)
instance ToSchema ReportId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)
instance ToParamSchema ReportId

newtype UserId = UserId { unUserId :: Int64 }
  deriving (Generic, Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Default, Arbitrary, PersistableWidth
           , LiteralSQL, FromSql SqlValue, ToSql SqlValue, FromHttpApiData, Num
           , HasColumnConstraint NotNull)
instance ToSchema UserId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)
instance ToParamSchema UserId

newtype UserImageId = UserImageId { unUserImageId :: Int64 }
  deriving (Generic, Eq, Show, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Default, Arbitrary, PersistableWidth
           , LiteralSQL, FromSql SqlValue, ToSql SqlValue, FromHttpApiData, Num
           , HasColumnConstraint NotNull)
instance ToSchema UserImageId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)
instance ToParamSchema UserImageId

getField' :: String -> (String, TypeQ) -> (String, TypeQ)
getField' "follow" (c@"id", _) = (c, [t|FollowId|])
getField' "like" (c@"id", _) = (c, [t|LikeId|])
getField' "notification" (c@"id", _) = (c, [t|NotificationId|])
getField' "owner_key" (c@"id", _) = (c, [t|OwnerKeyId|])
getField' "owner_token" (c@"id", _) = (c, [t|OwnerTokenId|])
getField' "owner_user" (c@"id", _) = (c, [t|OwnerUserId|])
getField' "post" (c@"id", _) = (c, [t|PostId|])
getField' "post_image" (c@"id", _) = (c, [t|PostImageId|])
getField' "question" (c@"id", _) = (c, [t|QuestionId|])
getField' "question_user" (c@"id", _) = (c, [t|QuestionUserId|])
getField' "report" (c@"id", _) = (c, [t|ReportId|])
getField' "user" (c@"id", _) = (c, [t|UserId|])
getField' "user_image" (c@"id", _) = (c, [t|UserImageId|])
getField' "follow" (c@"to_user_id", _) = (c, [t|UserId|])
getField' "follow" (c@"user_id", _) = (c, [t|UserId|])
getField' "like" (c@"post_id", _) = (c, [t|PostId|])
getField' "like" (c@"user_id", _) = (c, [t|UserId|])
getField' "notification" (c@"ref_post_id", _) = (c, [t|Maybe PostId|])
getField' "notification" (c@"ref_user_id", _) = (c, [t|Maybe UserId|])
getField' "notification" (c@"user_id", _) = (c, [t|UserId|])
getField' "owner_user" (c@"user_id", _) = (c, [t|UserId|])
getField' "post" (c@"mention_to", _) = (c, [t|Maybe UserId|])
getField' "post" (c@"reply_to", _) = (c, [t|Maybe PostId|])
getField' "post" (c@"user_id", _) = (c, [t|UserId|])
getField' "post_image" (c@"post_id", _) = (c, [t|PostId|])
getField' "question_user" (c@"question_id", _) = (c, [t|QuestionId|])
getField' "question_user" (c@"user_id", _) = (c, [t|UserId|])
getField' "report" (c@"target_user_id", _) = (c, [t|UserId|])
getField' "report" (c@"user_id", _) = (c, [t|UserId|])
getField' "user_image" (c@"user_id", _) = (c, [t|UserId|])
getField' _ x = x
