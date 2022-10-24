module Main where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Data.Function ((&))

main :: IO ()
main = defaultMain tui initialSt

-- placeholders for the App type variables
type Name = ()
type St = ()

initialSt :: St
initialSt = ()

tui :: App St e Name
tui = simpleApp homePage

homePage :: Widget Name
homePage = navigation
           <=>
           banner
           <=>
           content
           <=>
           footer

navigation :: Widget Name
navigation =
  padTopBottom 1 $
  limitWidthAndCenter $
  (str "conduit" & padRight Max) <+>
  str "Home   Sign in   Sign up"  

banner :: Widget Name
banner =
  vBox [ B.hBorder &
         padBottom (Pad 2)
       , C.hCenter $ str "conduit" &
         padBottom (Pad 1)
       , C.hCenter $ str "a place to share your knowledge"
       , padTop (Pad 2) $
         B.hBorder
       ]

content :: Widget Name
content =
  limitWidthAndCenter $
  feed <+> hLimitPercent 25 tags

feed :: Widget Name
feed =
  B.border $
  C.hCenter (str "<global feed>")
  <=> fill ' '

tags :: Widget Name
tags =
  B.border $
  vLimit 10 $
  C.hCenter (str "<popular tags>")
  <=> fill ' '

footer :: Widget Name
footer =
  padTop (Pad 1) $
  limitWidthAndCenter $
  str "conduit. An interactive learning project from Thinkster. Code & design licensed under MIT. Implemented by Amir Dekel."
  & padRight Max

limitWidthAndCenter :: Widget n -> Widget n
limitWidthAndCenter = C.hCenter . hLimit 120
