module TUI.Common.Links
  ( link, conduit
  , updateStLinks
  )
  where

import Brick
import qualified Brick.Focus as F

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
updateStLinks ls st = st { links = ls
                         , focus = F.focusRing $ getName <$> ls
                         }
