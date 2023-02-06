{-# LANGUAGE OverloadedStrings #-}
module Env.Defaults
  ( conduitDemoBaseUrl
  , conduitLocalBaseUrl
  , postgresLocalSettings
  ) where

import Hasql.Connection (Settings, settings)
import Servant.Client (BaseUrl(..), Scheme(..))

conduitDemoBaseUrl :: BaseUrl
conduitDemoBaseUrl = BaseUrl Https "api.realworld.io" 443 ""

conduitLocalBaseUrl :: BaseUrl
conduitLocalBaseUrl = BaseUrl Http "localhost" 8000 ""

postgresLocalSettings :: Settings
postgresLocalSettings = settings "localhost" 5432 "postgres" "fakeworld" "postgres"
