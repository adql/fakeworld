{-# LANGUAGE RecordWildCards #-}

module Server
  ( app
  ) where

import Data.Text (Text)
import Network.Wai
import Servant

import API.Request.Types
import API.Response.Types
import Server.DB
import Server.DummyDB

server :: Server API
server = serveArticles
    :<|> serveArticle
    :<|> serveComments
    :<|> serveProfile
    :<|> serveTags

serveArticles :: Maybe Tag
              -> Maybe Text
              -> Maybe Text
              -> Maybe Int
              -> Maybe Int
              -> Handler Articles
serveArticles quTagged quByAuthor quFavoritedBy quLimit quOffset =
  dbQueryArticles $ QueryArticles {..}

serveArticle :: Text -> Handler Article'
serveArticle = dbGetMaybe . getArticle

serveComments :: Text -> Handler Comments
serveComments = dbQueryComments

serveProfile :: Text -> Handler Profile'
serveProfile = dbQueryProfile

serveTags :: Handler Tags
serveTags = return dummyTags

userAPI :: Proxy API
userAPI = Proxy

app :: Application
app = serve userAPI server
