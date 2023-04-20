module TUI.Pages
  ( servePage
  ) where

import Brick

import TUI.Pages.Article
import TUI.Pages.HomePage
import TUI.Pages.NotImplemented
import TUI.Types

servePage :: St -> Widget Name
servePage st  = case stCurrentPage st of
  HomePage               -> homePage st
  ArticlePage            -> articlePage st
  NotImplementedPage msg -> notImplementedPage msg st --for development
  _                      -> undefined
