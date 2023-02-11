{-# LANGUAGE OverloadedStrings #-}
module Env.Defaults
  ( conduitDemoBaseUrl
  , conduitLocalBaseUrl
  ) where

import Servant.Client (BaseUrl(..), Scheme(..))

conduitDemoBaseUrl :: BaseUrl
conduitDemoBaseUrl = BaseUrl Https "api.realworld.io" 443 ""

conduitLocalBaseUrl :: BaseUrl
conduitLocalBaseUrl = BaseUrl Http "localhost" 8000 ""
