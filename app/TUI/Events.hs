{-# LANGUAGE OverloadedStrings #-}
module TUI.Events
  ( populateArticles
  , populateTags
  ) where

import Brick
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

import API.Request
import API.Response.Types
import TUI.Types

populateArticles :: EventM Name St [Article]
populateArticles = do
  offset <- homeArticleOffset <$> get
  articles' <- liftIO $ requestArticleList [("limit", Just "10"),
                                           ("offset", Just $ offset)]
  case articles' of
    Left _ -> return []
    Right (Articles articles) -> return articles

populateTags :: EventM Name St [Text]
populateTags = do
  tags' <- liftIO $ requestTags
  case tags' of
    Left _ -> return []
    Right (Tags tags) -> return tags
