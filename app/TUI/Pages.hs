module TUI.Pages
  ( mainViewport
  , serveMainWidget
  ) where

import Brick

import TUI.Pages.HomePage
import TUI.Types

mainViewport :: Widget Name -> Widget Name
mainViewport = viewport MainViewport Vertical

serveMainWidget :: St -> Widget Name
serveMainWidget st  = case currentPage st of
  HomePage -> homePage st
  _        -> undefined
