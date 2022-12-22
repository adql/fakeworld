module TUI.Common
  ( conduit
  , articleTags
  , authorBox
  , separator
  ) where

import Brick
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Border.Style (unicodeRounded)
import Data.Function ((&))
import Data.List (intersperse)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import API.Response.Types
import TUI.Style
import TUI.Types

conduit :: Widget n
conduit = withAttr conduitAttr (str "conduit")

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
