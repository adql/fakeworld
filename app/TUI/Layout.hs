module TUI.Layout
  ( page
  , limitWidthAndCenter
  , bodyWidth
  , commentSectionWidth
  , footer
  ) where

import Brick
import qualified Brick.Widgets.Center as C
import Data.Function ((&))

import TUI.Common
import TUI.Style
import TUI.Types

page :: St
     -> (St -> Widget Name)
     -> (St -> Widget Name)
     -> Widget Name
page st banner content =
  vBox [ navigation
       , banner st &
         padBottom (Pad 1)
       , content st
       -- the footer is rendered directly in the Pages.mainViewport to
       -- make it stick to the bottom
       ]

navigation :: Widget Name
navigation =
  padTopBottom 1 $
  limitWidthAndCenter bodyWidth $
  (conduit & padRight Max) <+>
  str "Home   Sign in   Sign up"  

footer :: Widget Name
footer =
  withDefAttr footerAttr $
  padTopBottom 1 $
  limitWidthAndCenter bodyWidth $
  conduit <+> str "  An interactive learning project from Thinkster. Code & design licensed under MIT. Implemented by Amir Dekel."
  & padRight Max

limitWidthAndCenter :: Int -> Widget n -> Widget n
limitWidthAndCenter w = C.hCenter . hLimit w

bodyWidth,
  commentSectionWidth
  :: Int
bodyWidth = 120
commentSectionWidth = bodyWidth * 2 `div` 3
