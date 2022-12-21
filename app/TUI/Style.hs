module TUI.Style
  ( theMap

  , authorBoxNameAttr
  , authorBoxTimeAttr
  , conduitAttr
  , feedSepAttr
  , homepageBannerAttr
  , previewDescAttr
  , previewFooterAttr
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

authorBoxNameAttr,
  authorBoxTimeAttr,
  conduitAttr,
  feedSepAttr,
  homepageBannerAttr,
  previewDescAttr,
  previewFooterAttr,
  previewHeadingAttr
  :: AttrName
authorBoxNameAttr = attrName "conduitGreen" <> attrName "aurhorBoxName"
authorBoxTimeAttr = attrName "pale" <> attrName "authorBoxTime"
conduitAttr = attrName "conduitGreen" <> attrName "conduitAttr"
feedSepAttr = attrName "pale" <> attrName "feedSep"
homepageBannerAttr = attrName "homepageBanner"
previewDescAttr = attrName "pale" <> attrName "previewDesc"
previewFooterAttr = attrName "pale" <> attrName "previewFooter"
previewHeadingAttr = attrName "previewHeading"

style :: Style -> Attr
style = withStyle defAttr

conduitGreen :: Color
conduitGreen = RGBColor 92 184 92
