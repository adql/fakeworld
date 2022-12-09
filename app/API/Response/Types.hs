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

data Profile = Profile
  { username  :: Text
  , bio       :: Maybe Text
  , image     :: Maybe Text
  , following :: Bool
  } deriving (Generic, Show)

instance FromJSON Profile
instance ToJSON Profile

newtype Profile' = Profile' { profile :: Profile }
  deriving (Generic, Show)

instance FromJSON Profile'
instance ToJSON Profile'

data User = User
  { email    :: Text
  , token    :: Text
  , username :: Text
  , bio      :: Maybe Text
  , image    :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON User
instance ToJSON User

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
  { slug :: Text
  , title :: Text
  , description :: Text
  , body :: Text
  , tagList :: [Tag]
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , favorited :: Bool
  , favoritesCount :: Int
  , author :: Profile
  } deriving (Generic, Show)

instance FromJSON Article
instance ToJSON Article

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
  { id :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , body :: Text
  , author :: Profile
  } deriving (Generic, Show)

instance FromJSON Comment
instance ToJSON Comment

newtype Comment' = Comment' { comment :: Comment }
  deriving (Generic, Show)

instance FromJSON Comment'
instance ToJSON Comment'

newtype Comments = Comments {comments :: [Comment] }
  deriving (Generic, Show)

instance FromJSON Comments
instance ToJSON Comments
