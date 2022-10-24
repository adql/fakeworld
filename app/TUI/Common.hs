module TUI.Common
  ( conduit
  , limitWidthAndCenter
  ) where

import Brick
import qualified Brick.Widgets.Center as C

limitWidthAndCenter :: Widget n -> Widget n
limitWidthAndCenter = C.hCenter . hLimit 120

conduit :: Widget n
conduit = withAttr (attrName "conduit") (str "conduit")
