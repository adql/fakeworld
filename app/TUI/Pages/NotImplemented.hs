module TUI.Pages.NotImplemented
  (notImplementedPage
  ) where

-- A temporary page for not implemented parts of the TUI

import Brick
import qualified Brick.Widgets.Center as C

import TUI.Layout
import TUI.Style
import TUI.Types

notImplementedPage :: St -> Widget Name
notImplementedPage st = page st banner content

banner :: St -> Widget Name
banner _ =
  withAttr homepageBannerAttr $
  padTopBottom 2 $ C.hCenter $ str "Not Yet Implemented"

content :: St -> Widget Name
content _ =
  limitWidthAndCenter bodyWidth $
  str "This is a work in progress..."
