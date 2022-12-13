{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module API.Request.Types
  ( API
  , ConduitRequest
  , ConduitResponse
  , Endpoint
  , ListArticles
  , GetTags
  , runConduitRequest
  ) where

import Data.ByteString (ByteString)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Text
import Network.HTTP.Simple (JSONException)
import Servant

import API.Response.Types
import Env

type Endpoint = ByteString

type ConduitRequest a = ReaderT Env IO a

type ConduitResponse a = Either JSONException a

runConduitRequest :: Env -> ConduitRequest (ConduitResponse a)
                  -> IO (ConduitResponse a)
runConduitRequest = flip runReaderT

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
