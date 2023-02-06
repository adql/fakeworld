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
serveArticles tag author favorited limit offset =
  dbGet $ getArticles (tag, author, favorited) limit offset

serveArticle :: Text -> Handler Article'
serveArticle = dbGetMaybe . getArticle

serveComments :: Text -> Handler Comments
serveComments = dbGet . getComments

serveProfile :: Text -> Handler Profile'
serveProfile = dbGetMaybe . getProfile

serveTags :: Handler Tags
serveTags = dbGet getAllTags

userAPI :: Proxy API
userAPI = Proxy

app :: Application
app = serve userAPI server
