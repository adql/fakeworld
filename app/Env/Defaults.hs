{-# LANGUAGE OverloadedStrings #-}
module Env.Defaults
  ( aPIBasePath
  , conduitDemoBaseUrl
  , conduitLocalBaseUrl
  ) where

import Data.ByteString (ByteString)
import Servant.Client

conduitDemoBaseUrl :: BaseUrl
conduitDemoBaseUrl = BaseUrl Https "api.realworld.io" 443 ""

conduitLocalBaseUrl :: BaseUrl
conduitLocalBaseUrl = BaseUrl Http "localhost" 8000 ""

aPIBasePath :: ByteString
aPIBasePath = "/api"
