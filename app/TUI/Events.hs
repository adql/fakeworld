{-# LANGUAGE OverloadedStrings #-}
module TUI.Events
  ( mainViewportEvent
  , openArticle
  , openHome
  ) where

import Brick
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Graphics.Vty.Input.Events (Event(..), Key(..))

import API.Request
import API.Request.Types (ConduitRequest, ConduitResponse, runConduitRequest)
import API.Response.Types
import TUI.Types

-- handleEvent :: BrickEvent Name e -> EventM Name St ()
-- handleEvent e@(VtyEvent ve) = case ve of
--   EvKey (KChar '\t') 

mainViewportEvent :: BrickEvent Name e -> EventM Name St ()
mainViewportEvent e@(VtyEvent ve) = case ve of
  EvKey KUp _ -> vScrollBy vp (-1)
  EvKey KDown _ -> vScrollBy vp 1
  EvKey KPageUp _ -> vScrollBy vp (-10) --todo: more sensible length
  EvKey KPageDown _ -> vScrollBy vp 10  --same
  EvKey KHome _ -> vScrollToBeginning vp
  EvKey KEnd _ -> vScrollToEnd vp
  _ -> resizeOrQuit e
  where
    vp = viewportScroll MainViewport
mainViewportEvent e = resizeOrQuit e

openHome :: EventM Name St ()
openHome = do
  artcls <- updateHomeArticles
  tgs <- updateAllTags
  modify $ \s -> s { currentPage = HomePage
                   , homeArticles = artcls
                   , allTags = tgs
                   }

openArticle :: ByteString -> EventM Name St ()
openArticle slug = do
  artcl <- updateArticle slug
  modify $ \s -> s { currentPage = ArticlePage
                   , articleCurrent = artcl
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

updateArticle :: ByteString -> EventM Name St (Maybe Article)
updateArticle slug = do
  artcl <- request $ requestArticle slug
  return $ either (const Nothing) (Just . article) artcl

request :: ConduitRequest (ConduitResponse a)
        -> EventM Name St (ConduitResponse a)
request rqst = do
  env' <- gets env
  liftIO $ runConduitRequest env' rqst
