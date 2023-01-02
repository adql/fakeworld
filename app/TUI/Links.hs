module TUI.Links
  ( link, conduit
  , updateStLinks
  , footerConduitLink
  , navConduitLink
  , navHomeLink
  , navSignInLink
  , navSignUpLink
  )
  where

import Brick
import Brick.Focus (FocusRing)
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

updateStLinks :: [Link] -> St -> St
updateStLinks ls st = st { links = permanentLinks <> ls
                         , focus = mkFocusRing ls
                         }

permanentLinks :: [Link]
permanentLinks = [ footerConduitLink
                 , navConduitLink
                 , navHomeLink
                 , navSignInLink
                 , navSignUpLink
                 ]

mkFocusRing :: [Link] -> FocusRing Name
mkFocusRing ls = F.focusRing
               $ NavConduit
               : NavHome
               : NavSignIn
               : NavSignUp
               : (linkName <$> ls)
               <> [FooterConduit]

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
navSignInLink = Link NavSignIn openNotImplemented "Sign in"
navSignUpLink = Link NavSignUp openNotImplemented "Sign up"
