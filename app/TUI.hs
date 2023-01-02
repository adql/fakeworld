{-# LANGUAGE OverloadedStrings #-}
module TUI
  ( initialSt
  , tui
  ) where

import Brick
import qualified Brick.Focus as F

import Env
import TUI.Common.Links
import TUI.Events
import TUI.Pages
import TUI.Style
import TUI.Types

initialSt :: Env -> Bool -> St
initialSt env' dark = updateStLinks [] $
  St { currentPage = HomePage
     , darkMode = dark
     , links = []
     , focus = F.focusRing []
     , homeArticleOffset = "0"
     , homeArticles = []
     , articleCurrent = Nothing
     , allTags = []
     , env = env'
     }

tui :: App St e Name
tui = App { appDraw = \st -> [mainViewport st $ serveMainWidget st]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = appEvent
          , appStartEvent = initiateApp
          , appAttrMap = theMap . darkMode
          }

initiateApp :: EventM Name St ()
initiateApp = openHome
