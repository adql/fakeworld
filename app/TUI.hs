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
import TUI.Style
import TUI.Types

initialSt :: Env -> St
initialSt env' = St
  { currentPage = HomePage
  , homeArticleOffset = "0"
  , homeArticles = []
  , articleCurrent = Nothing
  , allTags = []
  , env = env'
  }

tui :: App St e Name
tui = App { appDraw = \s -> [mainViewport $ serveMainWidget s]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = mainViewportEvent
          , appStartEvent = initiateApp
          , appAttrMap = const theMap
          }

initiateApp :: EventM Name St ()
initiateApp = openHome
