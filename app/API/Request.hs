module API.Request
  ( requestProfile
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

execJSONRequest :: FromJSON a => Request -> IO (Either JSONException a)
execJSONRequest request =
  httpJSONEither request >>= return . getResponseBody
