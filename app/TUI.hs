{-# LANGUAGE OverloadedStrings #-}
module TUI
  ( initialSt
  , tui
  ) where

import Brick
import qualified Brick.Focus as F

import Env
import TUI.Events
import TUI.Pages
import TUI.Style
import TUI.Types

initialSt :: Env -> Bool -> St
initialSt env' dark = 
  St { stCurrentPage = HomePage
     , stDarkMode = dark
     , stLinks = []
     , stFocus = F.focusRing []
     , stHomeArticleOffset = "0"
     , stHomeArticles = []
     , stArticleCurrent = Nothing
     , stAllTags = []
     , stEnv = env'
     }

tui :: App St e Name
tui = App { appDraw = \st -> [mainViewport st $ serveMainWidget st]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = appEvent
          , appStartEvent = initiateApp
          , appAttrMap = theMap . stDarkMode
          }

initiateApp :: EventM Name St ()
initiateApp = openHome
