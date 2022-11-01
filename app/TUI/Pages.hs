module TUI.Pages
  ( mainViewport
  ) where

import Brick

import TUI.Types

mainViewport :: Widget Name -> Widget Name
mainViewport = viewport MainViewport Vertical
