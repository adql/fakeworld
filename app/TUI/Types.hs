module TUI.Types
  ( Name(..)
  , Page(..)
  , St(..)
  ) where

import Data.ByteString (ByteString)

import API.Response.Types

data Name = MainViewport
  deriving (Show, Eq, Ord)

data St = St
  { currentPage :: Page
  , homeArticleOffset :: ByteString
  , homeArticles :: [Article]
  }

data Page = HomePage
          | LoginRegisterPage
          | ProfilePage
          | SettingsPage
          | CreateEditArticlePage
          | ArticlePage
