module TUI.Links
  ( link, conduit
  , footerConduitLink
  , navConduitLink
  , navHomeLink
  , navSignInLink
  , navSignUpLink
  )
  where

import Brick
import qualified Brick.Focus as F

import TUI.Events
import TUI.Style
import TUI.Types

link :: St -> Link -> Widget Name
link st = F.withFocusRing (focus st) $
  \focused l ->
    let attr = if focused then linkFocusedAttr else linkAttr
    in
      withAttr attr $ str $ linkText l

conduit :: St -> Link -> Widget Name
conduit st = overrideAttr linkAttr conduitAttr . link st

-- Links for the navbar and the footer

footerConduitLink,
  navConduitLink,
  navHomeLink,
  navSignInLink,
  navSignUpLink
  :: Link
footerConduitLink = Link FooterConduit openHome "conduit"
navConduitLink = Link NavConduit openHome "conduit"
navHomeLink = Link NavHome openHome "Home"
navSignInLink = Link NavSignIn (return ()) "Sign in"
navSignUpLink = Link NavSignUp (return ()) "Sign up"
