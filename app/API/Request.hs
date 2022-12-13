{-# LANGUAGE OverloadedStrings #-}
module API.Request
  ( requestArticle
  , requestArticleList
  , requestComments
  , requestProfile
  , requestTags
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.HTTP.Simple

import API.Response.Types
import API.Request.Types
import qualified API.Request.Endpoints as EP
import Env
import qualified Env.Defaults

requestProfile :: ByteString -> ConduitRequest (ConduitResponse Profile)
requestProfile name = do
  let path = mkPath [EP.profiles, name]
  r <- setRequestPath path <$> mkRequest
  execJSONRequest r

requestTags :: ConduitRequest (ConduitResponse Tags)
requestTags = do
  r <- setRequestPath (mkPath [EP.tags]) <$> mkRequest
  execJSONRequest r

requestArticleList :: Query -> ConduitRequest (ConduitResponse Articles)
requestArticleList query = do
  r <-  setRequestPath (mkPath [EP.articles])
    <$> setRequestQueryString query
    <$> mkRequest
  execJSONRequest r

requestArticle :: ByteString -> ConduitRequest (ConduitResponse Article)
requestArticle slug = do
  let path = mkPath [EP.articles, slug]
  r <- setRequestPath path <$> mkRequest
  execJSONRequest r

requestComments :: ByteString -> ConduitRequest (ConduitResponse Comments)
requestComments slug = do
  let path = mkPath [EP.articles, slug, "comments"]
  r <- setRequestPath path <$> mkRequest
  execJSONRequest r
  
mkRequest :: ConduitRequest Request
mkRequest = ask >>= \env ->
  let r = setRequestHost (requestHost env)
        $ setRequestPort (requestPort env)
        $ setRequestSecure (requestSecure env)
        $ defaultRequest
  in
    return r

mkPath :: [ByteString] -> ByteString
mkPath path = BS.intercalate "/" $ Env.Defaults.aPIBasePath : path

execJSONRequest :: FromJSON a => Request -> ConduitRequest (ConduitResponse a)
execJSONRequest r = liftIO $ 
  httpJSONEither r >>= return . getResponseBody
