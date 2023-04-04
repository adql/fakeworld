{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module API.Request.Types
  ( API
  , AuthenticateBody(..)
  ) where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant

import API.Common
import API.Response.Types

type API = APIBase :> (
             ListArticles
        :<|> GetArticle
        :<|> GetComments
        :<|> GetProfile
        :<|> GetTags
        :<|> Authenticate
             )

-- Endpoints

type APIBase = "api"

type ListArticles = "articles"
                 :> QueryParam "tag" Tag
                 :> QueryParam "author" Text
                 :> QueryParam "favorited" Text
                 :> QueryParam "limit" Int
                 :> QueryParam "offset" Int
                 :> Get '[JSON] Articles

type GetArticle = "articles"
               :> Capture "slug" Text
               :> Get '[JSON] Article'

type GetComments = "articles"
                :> Capture "slug" Text
                :> "comments"
                :> Get '[JSON] Comments

type GetProfile = "profiles"
               :> Capture "username" Text
               :> Get '[JSON] Profile'

type GetTags = "tags"
            :> Get '[JSON] Tags

type Authenticate = "users" :> "login"
                 :> ReqBody '[JSON] AuthenticateBody
                 :> Post '[JSON] User'

-- Request bodies

data AuthenticateBody = AuthenticateBody
  { authenticateEmail :: Text
  , authenticatePassword :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON AuthenticateBody where
  parseJSON = genericParseJSON $ withPrefixRemoval 12
                               $ asObjectWithSingleField "user"
  
instance ToJSON AuthenticateBody where
  toEncoding = genericToEncoding $ withPrefixRemoval 12
                                 $ asObjectWithSingleField "user"
