module TUI.Layout
  ( page
  , limitWidthAndCenter
  , bodyWidth
  ) where

import Brick
import qualified Brick.Widgets.Center as C
import Data.Function ((&))

import TUI.Common
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
       , footer
       ]

navigation :: Widget Name
navigation =
  padTopBottom 1 $
  limitWidthAndCenter bodyWidth $
  (conduit & padRight Max) <+>
  str "Home   Sign in   Sign up"  

footer :: Widget Name
footer =
  padTop (Pad 1) $
  limitWidthAndCenter bodyWidth $
  conduit <+> str "  An interactive learning project from Thinkster. Code & design licensed under MIT. Implemented by Amir Dekel."
  & padRight Max

limitWidthAndCenter :: Int -> Widget n -> Widget n
limitWidthAndCenter w = C.hCenter . hLimit w

bodyWidth :: Int
bodyWidth = 120
