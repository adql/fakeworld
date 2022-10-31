{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module API.Response.Types
  ( Profile(..)
  , Tags(..)
  , User(..)
  ) where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics

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

optionalUnwrapAndParse :: (Generic a, GFromJSON Zero (Rep a)) =>
                          Key.Key -> Value -> Parser a
optionalUnwrapAndParse key val0 = case val0 of
  Object o -> do
    val <- o .:? key .!= Object o
    genericParseJSON defaultOptions val
  val -> genericParseJSON defaultOptions val
