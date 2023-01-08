module TUI.Common.Links
  ( link
  , conduit
  , linkArticle
  , updateStLinks
  )
  where

import Brick
import qualified Brick.Focus as F
import Data.List (find)

import API.Response.Types
import TUI.Style
import TUI.Types

link :: St -> Link -> Widget Name
link st = F.withFocusRing (stFocus st) $
  \focused l ->
    let attr = if focused then linkFocusedAttr else linkAttr
        visible' w = if focused then visible w else w
    in
      withAttr attr $ visible' $ str $ linkText l

conduit :: St -> Link -> Widget Name
conduit st = overrideAttr linkAttr conduitAttr . link st

linkArticle :: St -> Article -> Widget Name
linkArticle st artcl =
  let name = LinkName $ articleSlug artcl in
    case find ((== name) . linkName) (stLinks st) of
      Just l -> link st l
      Nothing -> txt $ articleTitle artcl

updateStLinks :: [Link] -> St -> St
updateStLinks ls st = st { stLinks = ls
                         , stFocus = F.focusRing $ getName <$> ls
                         }
