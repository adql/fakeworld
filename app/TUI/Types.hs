module TUI.Types
  ( Name(..)
  , Page(..)
  , St(..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import API.Response.Types
import Env

data Name = MainViewport
  deriving (Show, Eq, Ord)

data St = St
  { currentPage :: Page
  , homeArticleOffset :: ByteString
  , homeArticles :: [Article]
  , articleCurrent :: Maybe Article
  , allTags :: [Text]
  , env :: Env
  }

data Page = HomePage
          | LoginRegisterPage
          | ProfilePage
          | SettingsPage
          | CreateEditArticlePage
          | ArticlePage
