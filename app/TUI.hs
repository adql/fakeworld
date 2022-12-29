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

initialSt :: Env -> St
initialSt env' = St
  { currentPage = HomePage
  , focus = F.focusRing baseFocusRing
  , homeArticleOffset = "0"
  , homeArticles = []
  , articleCurrent = Nothing
  , allTags = []
  , env = env'
  }

baseFocusRing :: [Name]
baseFocusRing = [ NavConduit
                , NavHome
                , NavSignIn
                , NavSignUp
                , FooterConduit
                ]

tui :: App St e Name
tui = App { appDraw = \st -> [mainViewport st $ serveMainWidget st]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = appEvent
          , appStartEvent = initiateApp
          , appAttrMap = const theMap
          }

initiateApp :: EventM Name St ()
initiateApp = openHome
