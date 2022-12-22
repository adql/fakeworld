{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module TUI.Pages.HomePage
  ( homePage
  ) where

import Brick
import qualified Brick.Widgets.Center as C
import Data.Function ((&))
import Data.List (intersperse)
import qualified Data.Text as T

import API.Response.Types

import TUI.Common
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
  limitWidthAndCenter $
  feed (homeArticles st) <+> hLimitPercent 25 (popularTags $ allTags st)

feed :: [Article] -> Widget Name
feed articles =
  padRight (Pad 1) $
  vBox $ intersperse feedSeparator $
  articlePreview <$> articles
  
articlePreview :: Article -> Widget Name
articlePreview article =
  articlePreviewHeader article
  <=>
  (withAttr previewHeadingAttr . txt . articleTitle) article
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

feedSeparator :: Widget Name
feedSeparator =
  withAttr feedSepAttr $
  vLimit 1 (fill '_') &
  padBottom (Pad 1)

-- todo: styling (after better structuring Style.hs)
popularTags :: [Tag] -> Widget Name
popularTags allTags =
  padLeft (Pad 1) $
  ( str "Popular Tags" &
    padBottom (Pad 1) )
  <=>
  -- Currently ugly running text since there's no trivial way to wrap
  -- an hBox -- https://github.com/jtdaugherty/brick/issues/400
  (txtWrap . T.concat . intersperse "  ") allTags
