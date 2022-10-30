module API.Request
  ( requestProfile
  , requestTags
  ) where

import Network.HTTP.Simple

import API.Defaults
import API.Response.Types
import qualified API.Request.Endpoints as EP

requestProfile :: String -> IO (Either JSONException Profile)
requestProfile name = do
  request <- parseRequest $ conduitDemoAPI <> EP.profiles <> name
  response <- httpJSONEither request
  return $ getResponseBody response

requestTags :: IO (Either JSONException Tags)
requestTags = do
  request <- parseRequest $ conduitDemoAPI <> EP.tags
  response <- httpJSONEither request
  return $ getResponseBody response

