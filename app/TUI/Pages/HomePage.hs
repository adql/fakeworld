{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TUI.Pages.HomePage
  ( homePage
  ) where

import Brick
import qualified Brick.Widgets.Border as B
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
  vBox $ intersperse (str " ") $
  articlePreview <$> articles
  
articlePreview :: Article -> Widget Name
articlePreview article =
  ( articlePreviewHeader article &
    padBottom (Pad 1) )
  <=>
  (txt . title) article
  <=>
  (txt . description) article
  <=>
  articlePreviewFooter article

articlePreviewHeader :: Article -> Widget Name
articlePreviewHeader article =
  authorBox article <+> padLeft Max likeBox

articlePreviewFooter :: Article -> Widget Name
articlePreviewFooter article =
  str "Read more..." <+> padLeft Max (tags $ tagList article)  
  
authorBox :: Article -> Widget Name
authorBox (Article {author = Profile {username}}) =
  withAttr (attrName "mainGreen") $ txt $ username

likeBox :: Widget Name
likeBox = emptyWidget

popularTags :: Widget Name
popularTags =
  B.border $
  vLimit 10 $
  C.hCenter (str "<popular tags>")
  <=> fill ' '

tags :: Tags -> Widget Name
tags (Tags ts) =
  hBox $ intersperse (str " ") $
  txt <$> ts
