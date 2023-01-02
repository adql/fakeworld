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
import Data.List (intersperse)

import TUI.Links
import TUI.Style
import TUI.Types

page :: St
     -> (St -> Widget Name)
     -> (St -> Widget Name)
     -> Widget Name
page st banner content =
  vBox [ navigation st
       , banner st &
         padBottom (Pad 1)
       , content st
       -- the footer is rendered directly in the Pages.mainViewport to
       -- make it stick to the bottom
       ]

navigation :: St -> Widget Name
navigation st =
  padTopBottom 1 $
  limitWidthAndCenter bodyWidth $
  (conduit st navConduitLink & padRight Max) <+>
  (hBox . intersperse (str "   ") . map (link st)) [ navHomeLink
                                                   , navSignInLink
                                                   , navSignUpLink
                                                   ]

footer :: St -> Widget Name
footer st =
  withDefAttr footerAttr $
  padTopBottom 1 $
  limitWidthAndCenter bodyWidth $
  conduit st footerConduitLink <+> str "  An interactive learning project from Thinkster. Code & design licensed under MIT. Implemented by Amir Dekel."
  & padRight Max

limitWidthAndCenter :: Int -> Widget n -> Widget n
limitWidthAndCenter w = C.hCenter . hLimit w

bodyWidth,
  commentSectionWidth
  :: Int
bodyWidth = 120
commentSectionWidth = bodyWidth * 2 `div` 3
