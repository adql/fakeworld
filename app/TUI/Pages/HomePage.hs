module TUI.Pages.HomePage
  ( homePage
  ) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

import TUI.Common
import TUI.Layout.Footer
import TUI.Layout.Header
import TUI.Types

homePage :: Widget Name
homePage = navigation
           <=>
           banner
           <=>
           content
           <=>
           footer

content :: Widget Name
content =
  limitWidthAndCenter $
  feed <+> hLimitPercent 25 tags

feed :: Widget Name
feed =
  B.border $
  C.hCenter (str "<global feed>")
  <=> vLimit 34 ( fill ' ' )

tags :: Widget Name
tags =
  B.border $
  vLimit 10 $
  C.hCenter (str "<popular tags>")
  <=> fill ' '
