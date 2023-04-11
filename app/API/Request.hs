module API.Request
  ( listArticles
  , getArticle
  , getComments
  , getProfile
  , getTags
  , getUser
  , authenticate
  )
  where

import Data.Text (Text)
import Servant
import Servant.Auth.Client
import Servant.Client

import API.Request.Types
import API.Response.Types

api :: Proxy API
api = Proxy

listArticles :: Maybe Text
             -> Maybe Text
             -> Maybe Text
             -> Maybe Int
             -> Maybe Int
             -> ClientM Articles
getArticle :: Text -> ClientM Article'
getComments :: Text -> ClientM Comments
getProfile :: Text -> ClientM Profile'
getTags :: ClientM Tags
getUser :: Token -> ClientM User'
authenticate :: AuthenticateBody -> ClientM User'
listArticles
  :<|> getArticle
  :<|> getComments
  :<|> getProfile
  :<|> getTags
  :<|> getUser
  :<|> authenticate
  = client api
