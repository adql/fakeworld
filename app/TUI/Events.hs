{-# LANGUAGE OverloadedStrings #-}
module TUI.Events
  ( appEvent
  , mainViewportEvent
  , openArticle
  , openHome
  ) where

import Brick
import Brick.Focus (FocusRing)
import qualified Brick.Focus as F
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Text (Text)
import Graphics.Vty.Input.Events (Event(..), Key(..))

import API.Request
import API.Request.Types (ConduitRequest, ConduitResponse, runConduitRequest)
import API.Response.Types
import TUI.Types

appEvent :: BrickEvent Name e -> EventM Name St ()
appEvent e@(VtyEvent ve) = case ve of
  EvKey KBackTab _     -> focusEvent F.focusPrev
  EvKey (KChar '\t') _ -> focusEvent F.focusNext
  EvKey KEnter _       -> openLink
  _                    -> mainViewportEvent e
appEvent e = resizeOrQuit e

focusEvent :: (FocusRing Name -> FocusRing Name)
           -> EventM Name St ()
focusEvent setter =
  modify $ \st -> st { focus = setter (focus st) }

openLink :: EventM Name St ()
openLink = do
  current <- F.focusGetCurrent <$> gets focus
  flip (maybe $ return ()) current $ \n -> do
    ls <- gets links
    maybe (return ()) linkHandler $
      find ((== n) . linkName) ls

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
