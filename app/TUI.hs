{-# LANGUAGE OverloadedStrings #-}
module TUI
  ( initialSt
  , tui
  ) where

import Brick
import qualified Brick.Focus as F
import Servant.Client (BaseUrl)

import TUI.Events
import TUI.Pages
import TUI.Style
import TUI.Types

initialSt :: BaseUrl -> Bool -> St
initialSt baseUrl dark = 
  St { stCurrentPage = HomePage
     , stDarkMode = dark
     , stLinks = []
     , stFocus = F.focusRing []
     , stFocusChanged = False
     , stArticles = []
     , stFilterOffset = 0
     , stFilterTag = Nothing
     , stArticleCurrent = Nothing
     , stAllTags = []
     , stBaseUrl = baseUrl
     }

tui :: App St e Name
tui = App { appDraw = \st -> [servePage st]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = appEvent
          , appStartEvent = initiateApp
          , appAttrMap = theMap . stDarkMode
          }

initiateApp :: EventM Name St ()
initiateApp = openHomeGlobal
