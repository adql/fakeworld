module TUI.Common
  ( conduit
  ) where

import Brick

conduit :: Widget n
conduit = withAttr (attrName "conduit") (str "conduit")
