{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server
  ( app
  ) where

import Data.Text (Text)
import Network.Wai
import Servant
import Servant.Auth.Server

import API.Request.Types
import API.Response.Types
import Server.DB

server :: Server API
server = serveArticles
    :<|> serveArticle
    :<|> serveComments
    :<|> serveProfile
    :<|> serveTags
    :<|> serveUser
    :<|> authenticate

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

serveUser :: AuthResult AuthenticatedUser -> Handler User'
serveUser _cred = notImplemented

authenticate :: AuthenticateBody -> Handler User'
authenticate _body = notImplemented

-- convenience for implementing the front-end first
notImplemented :: Handler a
notImplemented = throwError err418

userAPI :: Proxy API
userAPI = Proxy

app :: Application
app = serveWithContext userAPI cfg server
  where
    cfg = defaultCookieSettings :. jwtSettings :. EmptyContext
    -- Dummy JWT while authentication is still not implemented on the
    -- server side
    jwtSettings = defaultJWTSettings $ fromSecret "NOT IMPLEMENTED"
