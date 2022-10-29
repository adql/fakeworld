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
  parseJSON = unwrapParseJSON "profile"

data User = User
  { email    :: Text
  , token    :: Text
  , username :: Text
  , bio      :: Maybe Text
  , image    :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON User where
  parseJSON = unwrapParseJSON "user"

data Tags = Tags [Text] deriving (Generic, Show)

instance FromJSON Tags where
  parseJSON = unwrapParseJSON "tags"

unwrapParseJSON :: (Generic a, GFromJSON Zero (Rep a)) =>
                   Key.Key -> Value -> Parser a
unwrapParseJSON key = withObject (Key.toString key) $ \o -> do
  inner <- o .: key
  genericParseJSON defaultOptions inner
