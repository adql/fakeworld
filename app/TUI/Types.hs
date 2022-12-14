 {-# LANGUAGE MultiParamTypeClasses #-}
module TUI.Types
  ( Name(..)
  , Page(..)
  , St(..)
  , Link(..)
  ) where

import Brick
import Brick.Focus (FocusRing)
import Data.Text (Text)
import Servant.Client

import API.Response.Types

data Name = MainViewport

          -- Navigation and footer link widgets
          | NavConduit
          | NavHome
          | NavSignIn
          | NavSignUp
          | FooterConduit

          | LinkName Text
          
  deriving (Show, Eq, Ord)

data St = St
  { stCurrentPage :: Page
  , stDarkMode :: Bool
  , stLinks :: [Link]
  , stFocus :: FocusRing Name
  , stHomeArticleOffset :: Int
  , stHomeArticles :: [Article]
  , stArticleCurrent :: Maybe Article
  , stAllTags :: [Text]
  , stBaseUrl :: BaseUrl
  }

data Page = HomePage
          | LoginRegisterPage
          | ProfilePage
          | SettingsPage
          | CreateEditArticlePage
          | ArticlePage
          | NotImplementedPage --for development

data Link = Link
  { linkName :: Name
  , linkHandler :: EventM Name St ()
  , linkText :: String
  }

instance Named Link Name where
  getName = linkName
