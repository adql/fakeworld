module TUI
  ( initialSt
  , tui
  ) where

import Brick

import TUI.Pages
import TUI.Pages.HomePage
import TUI.Style
import TUI.Types

initialSt :: St
initialSt = ()

tui :: App St e Name
tui = App { appDraw = const [mainViewport homePage]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = resizeOrQuit
          , appStartEvent = return ()
          , appAttrMap = const theMap
          }
