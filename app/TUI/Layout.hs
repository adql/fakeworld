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

import TUI.Common
import TUI.Events
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
  (conduit st NavConduit & padRight Max) <+>
  (hBox . intersperse (str "   ") . map (link st)) [ navHomeLink
                                                   , navSignInLink
                                                   , navSignUpLink
                                                   ]
--  (link st navHomeLink) <+> str "   Sign in   Sign up"  

footer :: St -> Widget Name
footer st =
  withDefAttr footerAttr $
  padTopBottom 1 $
  limitWidthAndCenter bodyWidth $
  conduit st FooterConduit <+> str "  An interactive learning project from Thinkster. Code & design licensed under MIT. Implemented by Amir Dekel."
  & padRight Max

limitWidthAndCenter :: Int -> Widget n -> Widget n
limitWidthAndCenter w = C.hCenter . hLimit w

bodyWidth,
  commentSectionWidth
  :: Int
bodyWidth = 120
commentSectionWidth = bodyWidth * 2 `div` 3

-- Links

conduit :: St -> Name -> Widget Name
conduit st name = overrideAttr linkAttr conduitAttr $
  link st $ Link name openHome "conduit"

navHomeLink,
  navSignInLink,
  navSignUpLink
  :: Link
navHomeLink = Link NavHome openHome "Home"
navSignInLink = Link NavSignIn (return ()) "Sign in"
navSignUpLink = Link NavSignUp (return ()) "Sign up"
