module TUI.Style
  ( theMap
  ) where

import Brick
import qualified Graphics.Vty.Attributes as VA

theMap :: AttrMap
theMap = attrMap VA.defAttr
         [ (attrName "banner", VA.brightWhite `on` conduitGreen)
         , (attrName "conduit", VA.withStyle (fg conduitGreen) VA.bold)
         , (attrName "heading", VA.withStyle VA.defAttr VA.bold)
         , (attrName "mainGreen", fg conduitGreen)
         , (attrName "pale", VA.withStyle (fg VA.brightBlack) VA.dim)
         ]

conduitGreen :: VA.Color
conduitGreen = VA.RGBColor 92 184 92
