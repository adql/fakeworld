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
  , previewTitleAttr
  , separatorAttr
  ) where

import Brick
import Graphics.Vty.Attributes

theMap :: Bool -> AttrMap
theMap dark =
  let def = if dark then brightWhite `on` black
            else black `on` brightWhite
      footerBg = if dark then gray 0x10 else gray 0xF3
  in
    attrMap def
    [ -- general attributes
      (attrName "conduitGreen", fg conduitGreen)
    , (attrName "pale99", pale 0x99)    
    , (attrName "paleAA", pale 0xAA)
    , (attrName "paleBB", pale 0xBB)
    , (attrName "paleDD", pale 0xDD)
    
    -- element attributes
    , (articlePageBannerAttr, brightWhite `on` gray 0x33)
    , (articlePageBannerAuthorNameAttr, fg brightWhite)
    , (conduitAttr, style bold)
    , (footerAttr, bg footerBg)
    , (homepageBannerAttr, brightWhite `on` conduitGreen)
    , (linkFocusedAttr, currentAttr `withStyle` standout)
    , (previewTitleAttr, style bold)
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
  previewTitleAttr,
  separatorAttr
  :: AttrName
articlePageBannerAttr = attrName "articleBanner"
articlePageBannerAuthorNameAttr = attrName "articlePageBannerAuthorName"
articleTagsAttr = attrName "paleAA" <> attrName "articleTags"
articleTagsBorderAttr = attrName "paleDD" <> attrName "articleTagsBorder"
authorBoxNameAttr = attrName "conduitGreen" <> attrName "aurhorBoxName"
authorBoxTimeAttr = attrName "paleBB" <> attrName "authorBoxTime"
conduitAttr = attrName "conduitGreen" <> attrName "conduitAttr"
footerAttr = attrName "paleBB" <> attrName "footer"
homepageBannerAttr = attrName "homepageBanner"
linkAttr = attrName "conduitGreen" <> attrName "link"
linkFocusedAttr = linkAttr <> attrName "linkFocused"
previewDescAttr = attrName "pale99" <> attrName "previewDesc"
previewFooterAttr = attrName "paleBB" <> attrName "previewFooter"
previewTitleAttr = attrName "previewTitle"
separatorAttr = attrName "paleDD" <> attrName "feedSep"

style :: Style -> Attr
style = withStyle defAttr

pale :: Int -> Attr
pale = fg . gray

conduitGreen :: Color
conduitGreen = linearColor 92 184 (92::Int)

gray :: Int -> Color
gray v = linearColor v v v
