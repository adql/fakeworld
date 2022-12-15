module TUI.Style
  ( theMap

  , conduitAttr
  , homepageBannerAttr
  , previewHeadingAttr
  ) where

import Brick
import qualified Graphics.Vty.Attributes as VA

theMap :: AttrMap
theMap = attrMap VA.defAttr
         [ -- general attributes
           (attrName "conduitGreen", fg conduitGreen)
         , (attrName "pale", VA.withStyle (fg VA.brightBlack) VA.dim)

           -- element attributes
         , (conduitAttr, style VA.bold)
         , (homepageBannerAttr, VA.brightWhite `on` conduitGreen)
         , (previewHeadingAttr, style VA.bold)
         ]

conduitAttr,
  homepageBannerAttr,
  previewHeadingAttr
  :: AttrName
conduitAttr = attrName "conduitGreen" <> attrName "conduitAttr"
homepageBannerAttr = attrName "homepageBanner"
previewHeadingAttr = attrName "previewHeading"

style :: VA.Style -> VA.Attr
style = VA.withStyle VA.defAttr

conduitGreen :: VA.Color
conduitGreen = VA.RGBColor 92 184 92
