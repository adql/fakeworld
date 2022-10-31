{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module API.Response.Types
  ( Article(..)
  , Profile(..)
  , Tags(..)
  , User(..)
  ) where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics
import Data.Time.Clock (UTCTime)

data Profile = Profile
  { username  :: Text
  , bio       :: Maybe Text
  , image     :: Maybe Text
  , following :: Bool
  } deriving (Generic, Show)

instance FromJSON Profile where
  parseJSON = optionalUnwrapAndParse "profile"

data User = User
  { email    :: Text
  , token    :: Text
  , username :: Text
  , bio      :: Maybe Text
  , image    :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON User where
  parseJSON = optionalUnwrapAndParse "user"

data Tags = Tags [Text] deriving (Generic, Show)

instance FromJSON Tags where
  parseJSON = optionalUnwrapAndParse "tags"

data Article = Article
  { slug :: Text
  , title :: Text
  , description :: Text
  , body :: Text
  , tagList :: Tags
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , favorited :: Bool
  , favoritesCount :: Int
  , author :: Profile
  } deriving (Generic, Show)

instance FromJSON Article where
  parseJSON = optionalUnwrapAndParse "article"

data Articles = Articles [Article]
  deriving (Generic, Show)

instance FromJSON Articles where
  parseJSON = optionalUnwrapAndParse "articles"

data Comment = Comment
  { id :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , body :: Text
  , author :: Profile
  } deriving (Generic, Show)

instance FromJSON Comment where
  parseJSON = optionalUnwrapAndParse "comment"

data Comments = Comments [Comment]
  deriving (Generic, Show)

instance FromJSON Comments where
  parseJSON = optionalUnwrapAndParse "comments"

optionalUnwrapAndParse :: (Generic a, GFromJSON Zero (Rep a)) =>
                          Key.Key -> Value -> Parser a
optionalUnwrapAndParse key val0 = case val0 of
  Object o -> do
    val <- o .:? key .!= Object o
    genericParseJSON defaultOptions val
  val -> genericParseJSON defaultOptions val
