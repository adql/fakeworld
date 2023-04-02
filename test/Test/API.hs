-- Auxiliary module for easy testing of API functionality in REPL

module Test.API
  ( module API.Request
  , module API.Request.Types
  , testRequestDemo
  , testRequestLocal
  ) where

import Network.HTTP.Client.Conduit (newManager)
import Servant.Client

import API.Request
import API.Request.Types
import Env.Defaults

testRequestDemo :: ClientM a -> IO (Either ClientError a)
testRequestDemo = testRequest conduitDemoBaseUrl

testRequestLocal :: ClientM a -> IO (Either ClientError a)
testRequestLocal = testRequest conduitLocalBaseUrl

testRequest :: BaseUrl -> ClientM a -> IO (Either ClientError a)
testRequest baseUrl rqst = do
  manager' <- newManager
  runClientM rqst (mkClientEnv manager' baseUrl)
