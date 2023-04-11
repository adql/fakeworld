module TUI.Pages
  ( mainViewport
  , serveMainWidget
  ) where

import Brick
import Graphics.Vty.Image

import TUI.Layout
import TUI.Pages.Article
import TUI.Pages.HomePage
import TUI.Pages.NotImplemented
import TUI.Types

mainViewport :: St -> Widget Name -> Widget Name
mainViewport st w = Widget Fixed Fixed $ do
  -- Adding padding in order to make the footer stick to the bottom on
  -- short pages; the gap is calculated manually because using a
  -- greedy padding is not possible within a viewport
  h <- availHeight <$> getContext
  wH <- (imageHeight . image) <$> render w
  fH <- (imageHeight . image) <$> render (footer st)
  let gap = h - wH - fH
      padding = Pad $ if gap > 0 then gap else 0
  render $ viewport MainViewport Vertical $
    w <=> padTop padding (footer st)

serveMainWidget :: St -> Widget Name
serveMainWidget st  = case stCurrentPage st of
  HomePage               -> homePage st
  ArticlePage            -> articlePage st
  NotImplementedPage msg -> notImplementedPage msg st --for development
  _                      -> undefined
