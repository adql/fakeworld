{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module API.Request.Types
  ( API
  , Endpoint
  , ListArticles
  , GetTags
  ) where

import Data.Text
import Servant

import API.Response.Types

type Endpoint = String

type API = ListArticles
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

type GetTags = APIBase
            :> "tags"
            :> Get '[JSON] Tags
