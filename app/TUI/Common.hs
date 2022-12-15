module TUI.Common
  ( conduit
  ) where

import Brick

import TUI.Style

conduit :: Widget n
conduit = withAttr conduitAttr (str "conduit")
