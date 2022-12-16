{-# LANGUAGE OverloadedStrings #-}
module TUI
  ( initialSt
  , tui
  ) where

import Brick
import Graphics.Vty.Input.Events (Event(..), Key(..))

import Env
import TUI.Events
import TUI.Pages
import TUI.Pages.HomePage
import TUI.Style
import TUI.Types

initialSt :: Env -> St
initialSt env' = St
  { currentPage = HomePage
  , homeArticleOffset = "0"
  , homeArticles = []
  , allTags = []
  , env = env'
  }

tui :: App St e Name
tui = App { appDraw = \s -> [mainViewport $ homePage s]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = mainViewportHandleEvent
          , appStartEvent = initiateApp
          , appAttrMap = const theMap
          }

initiateApp :: EventM Name St ()
initiateApp = openHome

mainViewportHandleEvent :: BrickEvent Name e -> EventM Name St ()
mainViewportHandleEvent e0@(VtyEvent e) = case e of
  EvKey KUp _ -> vScrollBy vp (-1)
  EvKey KDown _ -> vScrollBy vp 1
  EvKey KPageUp _ -> vScrollBy vp (-10) --todo: more sensible length
  EvKey KPageDown _ -> vScrollBy vp 10  --same
  EvKey KHome _ -> vScrollToBeginning vp
  EvKey KEnd _ -> vScrollToEnd vp
  _ -> resizeOrQuit e0
  where
    vp = viewportScroll MainViewport
mainViewportHandleEvent e0 = resizeOrQuit e0
