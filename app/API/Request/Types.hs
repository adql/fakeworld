{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module API.Request.Types
  ( API
  , ListArticles
  , GetTags
  ) where

import Data.Text
import Servant

import API.Response.Types

type API = ListArticles
      :<|> GetArticle
      :<|> GetComments
      :<|> GetProfile
      :<|> GetTags

type APIBase = "api"

type ListArticles = APIBase
                 :> "articles"
                 :> QueryParam "tag" Tag
                 :> QueryParam "author" Text
                 :> QueryParam "favorited" Text
                 :> QueryParam "limit" Int
                 :> QueryParam "offset" Int
                 :> Get '[JSON] Articles

type GetArticle = APIBase
               :> "articles"
               :> Capture "slug" Text
               :> Get '[JSON] Article'

type GetComments = APIBase
                :> "articles"
                :> Capture "slug" Text
                :> "comments"
                :> Get '[JSON] Comments

type GetProfile = APIBase
               :> "profiles"
               :> Capture "username" Text
               :> Get '[JSON] Profile'

type GetTags = APIBase
            :> "tags"
            :> Get '[JSON] Tags
