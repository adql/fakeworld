 {-# LANGUAGE MultiParamTypeClasses #-}
module TUI.Types
  ( Name(..)
  , Page(..)
  , St(..)
  , Link(..)
  ) where

import Brick
import Brick.Focus (FocusRing)
import Data.ByteString (ByteString)
import Data.Text (Text)

import API.Response.Types
import Env

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
  { currentPage :: Page
  , darkMode :: Bool
  , links :: [Link]
  , focus :: FocusRing Name
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
          | NotImplementedPage --for development

data Link = Link
  { linkName :: Name
  , linkHandler :: EventM Name St ()
  , linkText :: String
  }

instance Named Link Name where
  getName = linkName
