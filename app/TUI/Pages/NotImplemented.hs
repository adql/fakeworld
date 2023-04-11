module TUI.Pages.NotImplemented
  (notImplementedPage
  ) where

-- A temporary page for not implemented parts of the TUI

import Brick
import qualified Brick.Widgets.Center as C

import TUI.Layout
import TUI.Style
import TUI.Types

notImplementedPage :: String -> St -> Widget Name
notImplementedPage msg st = page st banner (content msg)

banner :: St -> Widget Name
banner _ =
  withAttr homepageBannerAttr $
  padTopBottom 2 $ C.hCenter $ str "Not Yet Implemented"

content :: String -> St -> Widget Name
content msg _ =
  limitWidthAndCenter bodyWidth $ str msg
