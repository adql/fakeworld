{-# LANGUAGE OverloadedStrings #-}
module TUI.Style
  ( theMap

  , articlePageBannerAttr
  , articlePageBannerAuthorNameAttr
  , articleTagsAttr
  , articleTagsBorderAttr
  , authorBoxNameAttr
  , authorBoxTimeAttr
  , conduitAttr
  , footerAttr
  , homepageBannerAttr
  , previewDescAttr
  , previewFooterAttr
  , previewHeadingAttr
  , separatorAttr
  ) where

import Brick
import Data.Word (Word8)
import Graphics.Vty.Attributes

theMap :: AttrMap
theMap = attrMap (black `on` brightWhite)
         [ -- general attributes
           (attrName "conduitGreen", fg conduitGreen)
         , (attrName "pale9" , pale 9 )
         , (attrName "pale10", pale 10)
         , (attrName "pale11", pale 11)
         , (attrName "pale13", pale 13)
         , (attrName "pale14", pale 14)

           -- element attributes
         , (articlePageBannerAttr, brightWhite `on` (RGBColor 51 51 51))
         , (articlePageBannerAuthorNameAttr, fg brightWhite)
         , (conduitAttr, style bold)
         , (footerAttr, bg $ RGBColor 243 243 243)
         , (homepageBannerAttr, brightWhite `on` conduitGreen)
         , (previewHeadingAttr, style bold)
         ]

articlePageBannerAttr,
  articlePageBannerAuthorNameAttr,
  articleTagsAttr,
  articleTagsBorderAttr,
  authorBoxNameAttr,
  authorBoxTimeAttr,
  conduitAttr,
  footerAttr,
  homepageBannerAttr,
  previewDescAttr,
  previewFooterAttr,
  previewHeadingAttr,
  separatorAttr
  :: AttrName
articlePageBannerAttr = attrName "articleBanner"
articlePageBannerAuthorNameAttr = attrName "articlePageBannerAuthorName"
articleTagsAttr = attrName "pale10" <> attrName "articleTags"
articleTagsBorderAttr = attrName "pale13" <> attrName "articleTagsBorder"
authorBoxNameAttr = attrName "conduitGreen" <> attrName "aurhorBoxName"
authorBoxTimeAttr = attrName "pale11" <> attrName "authorBoxTime"
conduitAttr = attrName "conduitGreen" <> attrName "conduitAttr"
footerAttr = attrName "pale11" <> attrName "footer"
homepageBannerAttr = attrName "homepageBanner"
previewDescAttr = attrName "pale9" <> attrName "previewDesc"
previewFooterAttr = attrName "pale11" <> attrName "previewFooter"
previewHeadingAttr = attrName "previewHeading"
separatorAttr = attrName "pale13" <> attrName "feedSep"

style :: Style -> Attr
style = withStyle defAttr

conduitGreen :: Color
conduitGreen = RGBColor 92 184 92

pale :: Word8 -> Attr
pale c = fg $ RGBColor c' c' c'
  where
    c' = c * 17
