{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module TUI.Pages.HomePage
  ( homePage
  ) where

import Brick
import qualified Brick.Widgets.Center as C
import Data.Function ((&))
import Data.List (intersperse)
import Data.Text (Text, cons)

import API.Response.Types

import TUI.Common
import TUI.Common.Links
import TUI.Layout
import TUI.Style
import TUI.Types

homePage :: St -> Widget Name
homePage st = page st (const banner) content

banner :: Widget Name
banner =
  withAttr homepageBannerAttr $
  padTopBottom 2 $
  vBox [ C.hCenter $ str "conduit" &
         padBottom (Pad 1)
       , C.hCenter $ str "a place to share your knowledge"
       ]

content :: St -> Widget Name
content st =
  limitWidthAndCenter bodyWidth $
  feed st <+> hLimitPercent 25 (popularTags $ stAllTags st)

feed :: St -> Widget Name
feed st =
  padRight (Pad 1) $ padTop (Pad 1) $
  nav st
  <=>
  vBox ( intersperse separator $
         articlePreview st <$> stHomeArticles st )
  
nav :: St -> Widget Name
nav st = let tag = (cons '#') <$> stHomeTag st in
  hBox $ [ navItem (maybe True (const False) tag) "Global Feed"
         , maybe emptyWidget (navItem True) tag
         , padTop (Pad 1) separator
         ]

navItem :: Bool -> Text -> Widget Name
navItem current title =
  let itemAttr = if current then feedNavItemCurrentAttr
                 else feedNavItemAttr
      borderBottom = if current
        then overrideAttr separatorAttr feedNavItemCurrentBorderAttr separator
        else separator
      width = textWidth title + 2
  in
    hLimit width $
    C.hCenter ( withAttr itemAttr $ txt title )
    <=>
    borderBottom

articlePreview :: St -> Article -> Widget Name
articlePreview st article =
  articlePreviewHeader article
  <=>
  (overrideAttr linkAttr previewTitleAttr . linkArticle st) article
  <=>
  (withAttr previewDescAttr . txt . articleDescription) article
  <=>
  articlePreviewFooter article

articlePreviewHeader :: Article -> Widget Name
articlePreviewHeader article =
  let username = profileUsername $ articleAuthor article
      createdAt = articleCreatedAt article
  in
    authorBox username createdAt
    <+> padLeft Max likeBox &
    padBottom (Pad 1)

articlePreviewFooter :: Article -> Widget Name
articlePreviewFooter article =
  withAttr previewFooterAttr $
  padTop (Pad 1) (str "Read more...") <+> padLeft Max (articleTags $ articleTagList article)  
  
likeBox :: Widget Name
likeBox = emptyWidget

-- todo: styling (after better structuring Style.hs)
popularTags :: [Tag] -> Widget Name
popularTags allTags =
  withAttr tagBoxBgAttr $
  padTopBottom 1 $
  padLeftRight 1 $
  ( str "Popular Tags" &
    padBottom (Pad 1) )
  <=>
  hWrap 2 1 tagWidgets
  where
    tagWidgets = withAttr tagBoxTagAttr . txt <$> allTags
