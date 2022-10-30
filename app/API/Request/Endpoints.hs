module API.Request.Endpoints
  ( profiles
  , tags
  ) where

import API.Request.Types

profiles :: Endpoint
profiles = "/profiles/"

tags :: Endpoint
tags = "/tags"
