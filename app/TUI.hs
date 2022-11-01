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
tui = ( simpleApp (mainViewport homePage) )
      { appAttrMap = const theMap }
