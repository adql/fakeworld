{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module TUI.Pages.Article
  ( articlePage
  ) where

import Brick
import qualified Brick.Widgets.Center as C
import Data.Function ((&))
import Data.Maybe (fromJust)

import API.Response.Types
import TUI.Common
import TUI.Layout
import TUI.Style
import TUI.Types

articlePage :: St -> Widget Name
articlePage st = case articleCurrent st of
  Just _ -> page st banner content
  Nothing -> emptyWidget --make some "404"-ish (for completeness,
                         --should actually never happen)

banner :: St -> Widget Name
banner (St { articleCurrent }) =
  let artcl = unsafeGetArticle articleCurrent
      username = profileUsername $ articleAuthor artcl
      createdAt = articleCreatedAt artcl
  in
    withDefAttr articlePageBannerAttr $
    padTopBottom 2 $
    limitWidthAndCenter bodyWidth $
    (padBottom (Pad 1) $ txtWrap $ articleTitle artcl)
    <=>
    ( overrideAttr authorBoxNameAttr articlePageBannerAuthorNameAttr $
      authorBox username createdAt )

content :: St -> Widget Name
content st@(St { articleCurrent }) =
  let artcl = unsafeGetArticle articleCurrent
  in
    limitWidthAndCenter bodyWidth $
    vBox [ (txtWrap $ articleBody artcl) & padBottom (Pad 2)
         , (articleTags $ articleTagList artcl) & padBottom (Pad 1)
         , separator & padBottom (Pad 1)
         , C.hCenter $ authorBox (profileUsername $ articleAuthor artcl) (articleCreatedAt artcl)
         , padTop (Pad 2) $ commentSection st
         ]

-- to be implemented
commentSection :: St -> Widget Name
commentSection _ =
  limitWidthAndCenter commentSectionWidth $
  str "Sign in or sign up to add comments on this article." & padRight Max

-- for use in functions only called on Article page 
unsafeGetArticle :: Maybe Article -> Article
unsafeGetArticle = fromJust
