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
  return $ either (const []) articles articles'

populateTags :: EventM Name St [Text]
populateTags = do
  tags' <- liftIO $ requestTags
  return $ either (const []) tags tags'
