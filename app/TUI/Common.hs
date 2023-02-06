{-# LANGUAGE OverloadedStrings #-}
module TUI.Common
  ( articleTags
  , authorBox
  , separator
  , paragraphs
  ) where

import Brick
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Border.Style (unicodeRounded)
import Data.Function ((&))
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import API.Response.Types
import TUI.Style
import TUI.Types

authorBox :: Text -> UTCTime -> Widget Name
authorBox username time =
  ( withAttr authorBoxNameAttr $ txt $ username )
  <=>
  ( withAttr authorBoxTimeAttr $ str $ formatTime defaultTimeLocale timeFormat time )
  where
    timeFormat = "%B %-d, %Y"

articleTags :: [Tag] -> Widget Name
articleTags ts =
  withBorderStyle unicodeRounded $
  overrideAttr (attrName "border") articleTagsBorderAttr $
  hBox $ intersperse (str " ") $
  B.border . withAttr articleTagsAttr . txt <$> ts

separator :: Widget Name
separator =
  withAttr separatorAttr $
  vLimit 1 (fill '_') &
  padBottom (Pad 1)

-- Parse `\n`; each simply creates a new widget in a vertical box
paragraphs :: Text -> Widget Name
paragraphs = vBox . map par . T.lines
  where
    par text = if text == "" then txt " " else txtWrap text
