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
     , stHomeArticleOffset = 0
     , stHomeArticles = []
     , stArticleCurrent = Nothing
     , stAllTags = []
     , stBaseUrl = baseUrl
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
