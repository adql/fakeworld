module TUI.Common
  ( limitWidthAndCenter
  ) where

import Brick
import qualified Brick.Widgets.Center as C

limitWidthAndCenter :: Widget n -> Widget n
limitWidthAndCenter = C.hCenter . hLimit 120
