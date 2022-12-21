module TUI.Common
  ( conduit
  , authorBox
  ) where

import Brick
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

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
