module TUI.Layout.Footer
  ( footer
  ) where

import Brick
import Data.Function ((&))

import TUI.Common
import TUI.Types

footer :: Widget Name
footer =
  padTop (Pad 1) $
  limitWidthAndCenter $
  conduit <+> str "  An interactive learning project from Thinkster. Code & design licensed under MIT. Implemented by Amir Dekel."
  & padRight Max
