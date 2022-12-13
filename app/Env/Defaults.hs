{-# LANGUAGE OverloadedStrings #-}
module Env.Defaults
  ( aPIBasePath
  , conduitDemoAPI
  , conduitLocalAPI
  ) where

import Data.ByteString (ByteString)

import Env

conduitDemoAPI :: Env
conduitDemoAPI = Env "api.realworld.io" 443 True

conduitLocalAPI :: Env
conduitLocalAPI = Env "localhost" 8000 False

aPIBasePath :: ByteString
aPIBasePath = "/api"
