{-# LANGUAGE RecordWildCards #-}

module Server
  ( app
  ) where

import Data.Text (Text)
import Network.Wai
import Servant

import API.Request.Types
import API.Response.Types
import Server.DummyDB

server :: Server API
server = serveArticles
    :<|> serveTags

serveArticles :: Maybe Tag
              -> Maybe Text
              -> Maybe Text
              -> Maybe Int
              -> Maybe Int
              -> Handler Articles
serveArticles quTagged quByAuthor quFavoritedBy quLimit quOffset =
  dbQueryArticles $ QueryArticles {..}

serveTags :: Handler Tags
serveTags = return dummyTags

userAPI :: Proxy API
userAPI = Proxy

app :: Application
app = serve userAPI server
