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
import Data.Text (Text)
import GHC.Generics
import Data.Time.Clock (UTCTime(..))

import API.Common

data Profile = Profile
  { profileUsername  :: Text
  , profileBio       :: Maybe Text
  , profileImage     :: Maybe Text
  , profileFollowing :: Bool
  } deriving (Eq, Generic, Show)

instance FromJSON Profile where
  parseJSON = genericParseJSON $ withPrefixRemoval 7 defaultOptions
              
instance ToJSON Profile where
  toEncoding = genericToEncoding $ withPrefixRemoval 7 defaultOptions

newtype Profile' = Profile' { profile :: Profile }
  deriving (Eq, Generic, Show)

instance FromJSON Profile'

instance ToJSON Profile' where
  toEncoding (Profile' inner) = pairs ("profile" .= inner)

data User = User
  { userEmail    :: Text
  , userToken    :: Text
  , userUsername :: Text
  , userBio      :: Maybe Text
  , userImage    :: Maybe Text
  } deriving (Eq, Generic, Show)

instance FromJSON User where
  parseJSON = genericParseJSON $ withPrefixRemoval 4 defaultOptions
              
instance ToJSON User where
  toEncoding = genericToEncoding $ withPrefixRemoval 4 defaultOptions

newtype User' = User' { user :: User }
  deriving (Eq, Generic, Show)

instance FromJSON User'

instance ToJSON User' where
  toEncoding (User' inner) = pairs ("user" .= inner)

type Tag = Text

newtype Tags = Tags { tags :: [Tag] }
  deriving (Eq, Generic, Show)

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
  } deriving (Eq, Generic, Show)

instance FromJSON Article where
  parseJSON = genericParseJSON $ withPrefixRemoval 7 defaultOptions

instance ToJSON Article where
  toEncoding = genericToEncoding $ withPrefixRemoval 7 defaultOptions

newtype Article' = Article' { article :: Article }
  deriving (Eq, Generic, Show)

instance FromJSON Article'

instance ToJSON Article' where
  toEncoding (Article' inner) = pairs ("article" .= inner)

data Articles = Articles { articles :: [Article]
                         , articlesCount :: Int
                         }
  deriving (Eq, Generic, Show)

instance FromJSON Articles

instance ToJSON Articles where
  toEncoding (Articles inner count) =
    pairs ("articles" .= inner <> "articlesCount" .= count)

data Comment = Comment
  { commentId :: Int
  , commentCreatedAt :: UTCTime
  , commentUpdatedAt :: UTCTime
  , commentBody :: Text
  , commentAuthor :: Profile
  } deriving (Eq, Generic, Show)

instance FromJSON Comment where
  parseJSON = genericParseJSON $ withPrefixRemoval 7 defaultOptions
              
instance ToJSON Comment where
  toEncoding = genericToEncoding $ withPrefixRemoval 7 defaultOptions

newtype Comment' = Comment' { comment :: Comment }
  deriving (Eq, Generic, Show)

instance FromJSON Comment'

instance ToJSON Comment' where
  toEncoding (Comment' inner) = pairs ("comment" .= inner)

newtype Comments = Comments {comments :: [Comment] }
  deriving (Eq, Generic, Show)

instance FromJSON Comments

instance ToJSON Comments where
  toEncoding (Comments inner) = pairs ("comments" .= inner)
