module TUI.Style
  ( theMap

  , conduitAttr
  , homepageBannerAttr
  , previewHeadingAttr
  ) where

import Brick
import Graphics.Vty.Attributes

theMap :: AttrMap
theMap = attrMap (black `on` brightWhite)
         [ -- general attributes
           (attrName "conduitGreen", fg conduitGreen)
         , (attrName "pale", withStyle (fg brightBlack) dim)

           -- element attributes
         , (conduitAttr, style bold)
         , (homepageBannerAttr, brightWhite `on` conduitGreen)
         , (previewHeadingAttr, style bold)
         ]

conduitAttr,
  homepageBannerAttr,
  previewHeadingAttr
  :: AttrName
conduitAttr = attrName "conduitGreen" <> attrName "conduitAttr"
homepageBannerAttr = attrName "homepageBanner"
previewHeadingAttr = attrName "previewHeading"

style :: Style -> Attr
style = withStyle defAttr

conduitGreen :: Color
conduitGreen = RGBColor 92 184 92
