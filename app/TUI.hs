module TUI
  ( initialSt
  , tui
  ) where

import Brick

import TUI.Pages.HomePage
import TUI.Style
import TUI.Types

initialSt :: St
initialSt = ()

tui :: App St e Name
tui = ( simpleApp homePage )
      { appAttrMap = const theMap }
