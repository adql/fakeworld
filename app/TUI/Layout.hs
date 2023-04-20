module TUI.Layout
  ( page
  , limitWidthAndCenter
  , bodyWidth
  , commentSectionWidth
  , footer
  , hWrap
  ) where

import Brick
import qualified Brick.Widgets.Center as C
import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.List (intersperse)
import Graphics.Vty.Image (imageWidth)

import TUI.Common.Links
import TUI.Style
import TUI.Types

page :: St
     -> (St -> Widget Name)
     -> (St -> Widget Name)
     -> Widget Name
page st banner content =
  vBox [ viewport MainViewport Vertical $ vBox
           [ navigation st
           , banner st &
             padBottom (Pad 1)
           , content st
           ]
       , footer st
       ]

navigation :: St -> Widget Name
navigation st =
  padTopBottom 1 $
  limitWidthAndCenter bodyWidth $
  (linkConduit st NavConduit & padRight Max) <+>
  (hBox . intersperse (str "   ") . map link') [ (NavHome, str "Home")
                                               , (NavSignIn, str "Sign in")
                                               , (NavSignUp, str "Sign up")
                                               ]
  where
    link' = uncurry $ link st

footer :: St -> Widget Name
footer st =
  withDefAttr footerAttr $
  padTopBottom 1 $
  limitWidthAndCenter bodyWidth $
  linkConduit st FooterConduit <+> str "  An interactive learning project from Thinkster. Code & design licensed under MIT. Implemented by Amir Dekel."
  & padRight Max

limitWidthAndCenter :: Int -> Widget n -> Widget n
limitWidthAndCenter w = C.hCenter . hLimit w

bodyWidth,
  commentSectionWidth
  :: Int
bodyWidth = 120
commentSectionWidth = bodyWidth * 2 `div` 3

-- Similar to txtWrap, but for widgets, and with additional gap
-- arguments; too long tags are cropped
hWrap :: Int -> Int -> [Widget n] -> Widget n
hWrap hGap vGap widgets = Widget Fixed Fixed $ do
  avlWdth0 <- availWidth <$> getContext
  let wrap (rows,wdgts,avlWdth) widget = do
        len <- imageWidth . image <$> render widget
        if null wdgts || len <= avlWdth
          then return (rows, widget:wdgts, avlWdth-len-hGap)
          else wrap ((mkRow wdgts):rows, [], avlWdth0) widget
  (rows',wdgts,_) <- foldlM wrap ([],[],avlWdth0) widgets
  let rows = intersperse (vLimit vGap $ fill ' ')
             $ (mkRow wdgts) : rows' --foldlM returns without adding
                                     --last row
  render $
    setAvailableSize (avlWdth0, length rows) $ vBox (reverse rows)
  where
    mkRow wdgts = hBox $ intersperse (hLimit hGap $ fill ' ')
                  (reverse wdgts)
