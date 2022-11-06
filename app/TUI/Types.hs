module TUI.Types
  ( Name(..)
  , Page(..)
  , St(..)
  ) where

data Name = MainViewport
  deriving (Show, Eq, Ord)

data St = St
  { currentPage :: Page
  , homeArticleOffset :: Int
  }

data Page = Home
          | LoginRegister
          | Profile
          | Settings
          | CreateEditArticle
          | Article
