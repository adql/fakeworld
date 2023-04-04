module API.Request
  ( listArticles
  , getArticle
  , getComments
  , getProfile
  , getTags
  , authenticate
  )
  where

import Data.Text (Text)
import Servant
import Servant.Client

import API.Request.Types
import API.Response.Types

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
authenticate :: AuthenticateBody -> ClientM User'
listArticles
  :<|> getArticle
  :<|> getComments
  :<|> getProfile
  :<|> getTags
  :<|> authenticate
  = client (Proxy :: Proxy API)
