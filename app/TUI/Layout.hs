module TUI.Layout
  ( page
  ) where

import Brick
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
  limitWidthAndCenter $
  (conduit & padRight Max) <+>
  str "Home   Sign in   Sign up"  

footer :: Widget Name
footer =
  padTop (Pad 1) $
  limitWidthAndCenter $
  conduit <+> str "  An interactive learning project from Thinkster. Code & design licensed under MIT. Implemented by Amir Dekel."
  & padRight Max
