{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module API.Request.Types
  ( API
  ) where

import Data.Text
import Servant

import API.Response.Types

type API = APIBase :> (
             ListArticles
        :<|> GetArticle
        :<|> GetComments
        :<|> GetProfile
        :<|> GetTags
             )

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
