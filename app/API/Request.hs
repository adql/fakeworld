module API.Request
  ( requestArticle
  , requestArticleList
  , requestComments
  , requestProfile
  , requestTags
  ) where

import Data.Aeson (FromJSON)
import Network.HTTP.Simple

import API.Defaults
import API.Response.Types
import qualified API.Request.Endpoints as EP

requestProfile :: String -> IO (Either JSONException Profile)
requestProfile name = do
  request <- parseRequest $ conduitDemoAPI <> EP.profiles <> name
  execJSONRequest request

requestTags :: IO (Either JSONException Tags)
requestTags = do
  request <- parseRequest $ conduitDemoAPI <> EP.tags
  execJSONRequest request

requestArticleList :: Query -> IO (Either JSONException Articles)
requestArticleList query = do
  request <- setRequestQueryString query <$>
             ( parseRequest $ conduitDemoAPI <> EP.articles )
  execJSONRequest request

requestArticle :: String -> IO (Either JSONException Article)
requestArticle slug' = do
  request <- parseRequest $ conduitDemoAPI <> EP.articles <> "/" <> slug'
  execJSONRequest request

requestComments :: String -> IO (Either JSONException Comments)
requestComments slug' = do
  request <- parseRequest $ conduitDemoAPI <> EP.articles <> "/" <> slug' <> "/comments"
  execJSONRequest request

execJSONRequest :: FromJSON a => Request -> IO (Either JSONException a)
execJSONRequest request =
  httpJSONEither request >>= return . getResponseBody
