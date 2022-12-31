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
  , linkAttr
  , linkFocusedAttr
  , previewDescAttr
  , previewFooterAttr
  , previewHeadingAttr
  , separatorAttr
  ) where

import Brick
import Data.Word (Word8)
import Graphics.Vty.Attributes

theMap :: Bool -> AttrMap
theMap dark =
  let def = if dark then brightWhite `on` black
            else black `on` brightWhite
      footerBg = if dark then RGBColor 15 15 15 else RGBColor 243 243 243
  in
    attrMap def
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
    , (footerAttr, bg footerBg)
    , (homepageBannerAttr, brightWhite `on` conduitGreen)
    , (linkFocusedAttr, currentAttr `withStyle` standout)
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
  linkAttr,
  linkFocusedAttr,
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
linkAttr = attrName "conduitGreen" <> attrName "link"
linkFocusedAttr = linkAttr <> attrName "linkFocused"
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
