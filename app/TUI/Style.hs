module TUI.Style
  ( theMap
  ) where

import Brick
import qualified Graphics.Vty.Attributes as VA

theMap :: AttrMap
theMap = attrMap VA.defAttr
         [ (attrName "banner", VA.brightWhite `on` conduitGreen)
         , (attrName "conduit", VA.withStyle (fg conduitGreen) VA.bold)
         ]

conduitGreen :: VA.Color
conduitGreen = VA.RGBColor 92 184 92
