{-# LANGUAGE OverloadedStrings #-}
module API.Request.Endpoints
  ( articles
  , profiles
  , tags
  ) where

import API.Request.Types (Endpoint)

profiles :: Endpoint
profiles = "profiles"

tags :: Endpoint
tags = "tags"

articles :: Endpoint
articles = "articles"
