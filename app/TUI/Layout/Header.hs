module TUI.Layout.Header
  ( navigation
  , banner
  ) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Data.Function ((&))

import TUI.Common
import TUI.Types

navigation :: Widget Name
navigation =
  padTopBottom 1 $
  limitWidthAndCenter $
  (str "conduit" & padRight Max) <+>
  str "Home   Sign in   Sign up"  

banner :: Widget Name
banner =
  vBox [ B.hBorder &
         padBottom (Pad 2)
       , C.hCenter $ str "conduit" &
         padBottom (Pad 1)
       , C.hCenter $ str "a place to share your knowledge"
       , padTop (Pad 2) $
         B.hBorder
       ]

