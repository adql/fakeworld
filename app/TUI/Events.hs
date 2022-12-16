{-# LANGUAGE OverloadedStrings #-}
module TUI.Events
  ( openHome
  ) where

import Brick
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

import API.Request
import API.Request.Types (ConduitRequest, ConduitResponse, runConduitRequest)
import API.Response.Types
import TUI.Types

openHome :: EventM Name St ()
openHome = do
  artcls <- updateHomeArticles
  tgs <- updateAllTags
  modify $ \s -> s { currentPage = HomePage
                   , homeArticles = artcls
                   , allTags = tgs
                   }

-- EventM actions to update state record fields

updateHomeArticles :: EventM Name St [Article]
updateHomeArticles = do
  offset <- homeArticleOffset <$> get
  articles' <- request $
               requestArticleList [("limit", Just "10"),
                                   ("offset", Just $ offset)]
  return $ either (const []) articles articles'

updateAllTags :: EventM Name St [Text]
updateAllTags = do
  tags' <- request requestTags
  return $ either (const []) tags tags'

request :: ConduitRequest (ConduitResponse a)
        -> EventM Name St (ConduitResponse a)
request rqst = do
  env' <- gets env
  liftIO $ runConduitRequest env' rqst
