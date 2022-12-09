{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module API.Response.Types
  ( Article(..), Article'(..)
  , Articles(..)
  , Comment(..), Comment'(..)
  , Comments(..)
  , Profile(..), Profile'(..)
  , Tag
  , Tags(..)
  , User(..), User'(..)
  ) where

import Data.Aeson
import Data.Char (toLower)
import Data.List (uncons)
import Data.Text (Text)
import GHC.Generics
import Data.Time.Clock (UTCTime(..))

data Profile = Profile
  { profileUsername  :: Text
  , profileBio       :: Maybe Text
  , profileImage     :: Maybe Text
  , profileFollowing :: Bool
  } deriving (Generic, Show)

instance FromJSON Profile where
  parseJSON = genericParseJSON $ withPrefixRemoval 7
              
instance ToJSON Profile where
  toEncoding = genericToEncoding $ withPrefixRemoval 7

newtype Profile' = Profile' { profile :: Profile }
  deriving (Generic, Show)

instance FromJSON Profile'
instance ToJSON Profile'

data User = User
  { userEmail    :: Text
  , userToken    :: Text
  , userUsername :: Text
  , userBio      :: Maybe Text
  , userImage    :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON User where
  parseJSON = genericParseJSON $ withPrefixRemoval 4
              
instance ToJSON User where
  toEncoding = genericToEncoding $ withPrefixRemoval 4

newtype User' = User' { user :: User }
  deriving (Show, Generic)

instance FromJSON User'
instance ToJSON User'

type Tag = Text

newtype Tags = Tags { tags :: [Tag] }
  deriving (Generic, Show)

instance FromJSON Tags
instance ToJSON Tags

data Article = Article
  { articleSlug :: Text
  , articleTitle :: Text
  , articleDescription :: Text
  , articleBody :: Text
  , articleTagList :: [Tag]
  , articleCreatedAt :: UTCTime
  , articleUpdatedAt :: UTCTime
  , articleFavorited :: Bool
  , articleFavoritesCount :: Int
  , articleAuthor :: Profile
  } deriving (Generic, Show)

instance FromJSON Article where
  parseJSON = genericParseJSON $ withPrefixRemoval 7

instance ToJSON Article where
  toEncoding = genericToEncoding $ withPrefixRemoval 7

newtype Article' = Article' { article :: Article }
  deriving (Generic, Show)

instance FromJSON Article'
instance ToJSON Article'

data Articles = Articles { articles :: [Article]
                         , articlesCount :: Int
                         }
  deriving (Generic, Show)

instance FromJSON Articles
instance ToJSON Articles

data Comment = Comment
  { commentId :: Int
  , commentCreatedAt :: UTCTime
  , commentUpdatedAt :: UTCTime
  , commentBody :: Text
  , commentAuthor :: Profile
  } deriving (Generic, Show)

instance FromJSON Comment where
  parseJSON = genericParseJSON $ withPrefixRemoval 7
              
instance ToJSON Comment where
  toEncoding = genericToEncoding $ withPrefixRemoval 7

newtype Comment' = Comment' { comment :: Comment }
  deriving (Generic, Show)

instance FromJSON Comment'
instance ToJSON Comment'

newtype Comments = Comments {comments :: [Comment] }
  deriving (Generic, Show)

instance FromJSON Comments
instance ToJSON Comments

withPrefixRemoval :: Int -> Options
withPrefixRemoval len = defaultOptions { fieldLabelModifier = unPrefix }
  where
    unPrefix =
      maybe "" (\(c,cs) -> toLower c : cs) . uncons . drop len
    
