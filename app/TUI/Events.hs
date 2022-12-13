{-# LANGUAGE OverloadedStrings #-}
module TUI.Events
  ( populateArticles
  , populateTags
  ) where

import Brick
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

import API.Request
import API.Request.Types (runConduitRequest)
import API.Response.Types
import TUI.Types

populateArticles :: EventM Name St [Article]
populateArticles = do
  offset <- homeArticleOffset <$> get
  env' <- gets env
  articles' <- liftIO $ runConduitRequest env' $
               requestArticleList [("limit", Just "10"),
                                   ("offset", Just $ offset)]
  return $ either (const []) articles articles'

populateTags :: EventM Name St [Text]
populateTags = do
  env' <- gets env
  tags' <- liftIO $ runConduitRequest env' requestTags
  return $ either (const []) tags tags'
