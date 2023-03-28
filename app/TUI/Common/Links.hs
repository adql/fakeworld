module TUI.Common.Links
  ( link
  , linkArticle
  , linkConduit
  , updateStLinks
  )
  where

import Brick
import qualified Brick.Focus as F
import Data.List (find)

import API.Response.Types
import TUI.Style
import TUI.Types

link :: St -> Name -> Widget Name -> Widget Name
link st name w = case getLink name st of
  Nothing -> w
  Just l -> flip ( F.withFocusRing $ stFocus st ) l $
    \focused _l ->
      let attr = if focused then linkFocusedAttr else linkAttr
          visibleIfFocused w' = if focused then visible w' else w'
      in
        withAttr attr $ visibleIfFocused w

linkConduit :: St -> Name -> Widget Name
linkConduit st name = overrideAttr linkAttr conduitAttr
                      $ link st name $ str "conduit"

linkArticle :: St -> Article -> Widget Name
linkArticle st artcl =
  let name = LinkName $ articleSlug artcl in
    link st name (txt $ articleTitle artcl)

updateStLinks :: [Link] -> St -> St
updateStLinks ls st = st { stLinks = ls
                         , stFocus = F.focusRing $ getName <$> ls
                         }

getLink :: Name -> St -> Maybe Link
getLink name = find ((== name) . linkName) . stLinks
