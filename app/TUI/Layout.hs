module TUI.Layout
  ( banner
  , footer
  , navigation
  ) where

import Brick
import qualified Brick.Widgets.Center as C
import Data.Function ((&))

import TUI.Common
import TUI.Types

navigation :: Widget Name
navigation =
  padTopBottom 1 $
  limitWidthAndCenter $
  (conduit & padRight Max) <+>
  str "Home   Sign in   Sign up"  

banner :: Widget Name
banner =
  withAttr (attrName "banner") $
  padTopBottom 2 $
  vBox [ C.hCenter $ str "conduit" &
         padBottom (Pad 1)
       , C.hCenter $ str "a place to share your knowledge"
       ]

footer :: Widget Name
footer =
  padTop (Pad 1) $
  limitWidthAndCenter $
  conduit <+> str "  An interactive learning project from Thinkster. Code & design licensed under MIT. Implemented by Amir Dekel."
  & padRight Max
