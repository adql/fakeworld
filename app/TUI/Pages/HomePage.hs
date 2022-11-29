{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TUI.Pages.HomePage
  ( homePage
  ) where

import Brick
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Border.Style (unicodeRounded)
import qualified Brick.Widgets.Center as C
import Data.Function ((&))
import Data.List (intersperse)

import API.Response.Types
import TUI.Common
import TUI.Layout.Footer
import TUI.Layout.Header
import TUI.Types

homePage :: St -> Widget Name
homePage st = navigation
              <=>
              banner
              <=>
              content st
              <=>
              footer

content :: St -> Widget Name
content st =
  limitWidthAndCenter $
  feed (homeArticles st) <+> hLimitPercent 25 popularTags

feed :: [Article] -> Widget Name
feed articles =
  padRight (Pad 1) $
  vBox $ intersperse feedSeparator $
  articlePreview <$> articles
  
articlePreview :: Article -> Widget Name
articlePreview article =
  articlePreviewHeader article
  <=>
  (txt . title) article
  <=>
  (txt . description) article
  <=>
  articlePreviewFooter article

articlePreviewHeader :: Article -> Widget Name
articlePreviewHeader article =
  authorBox article <+> padLeft Max likeBox &
  padBottom (Pad 1)

articlePreviewFooter :: Article -> Widget Name
articlePreviewFooter article =
  padTop (Pad 1) $
  str "Read more..." <+> padLeft Max (tags $ tagList article)  
  
authorBox :: Article -> Widget Name
authorBox (Article {author = Profile {username}}) =
  withAttr (attrName "mainGreen") $ txt $ username

likeBox :: Widget Name
likeBox = emptyWidget

feedSeparator :: Widget Name
feedSeparator =
  withAttr (attrName "pale") $
  vLimit 1 (fill '_') &
  padBottom (Pad 1)

popularTags :: Widget Name
popularTags =
  padLeft (Pad 1) $
  B.border $
  vLimit 10 $
  C.hCenter (str "<popular tags>")
  <=> fill ' '

tags :: Tags -> Widget Name
tags (Tags ts) =
  withBorderStyle unicodeRounded $
  overrideAttr (attrName "border") (attrName "pale") $
  withAttr (attrName "pale") $
  hBox $ intersperse (str " ") $
  B.border . txt <$> ts
