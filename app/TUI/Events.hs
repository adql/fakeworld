{-# LANGUAGE OverloadedStrings #-}
module TUI.Events
  ( appEvent
  , mainViewportEvent
  , openArticle
  , openHome
  , openNotImplemented

  , footerConduitLink
  , navConduitLink
  , navHomeLink
  , navSignInLink
  , navSignUpLink
  ) where

import Brick
import Brick.Focus (FocusRing)
import qualified Brick.Focus as F
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Text (Text)
import Graphics.Vty.Input.Events (Event(..), Key(..))

import API.Request
import API.Request.Types (ConduitRequest, ConduitResponse, runConduitRequest)
import API.Response.Types
import TUI.Common.Links
import TUI.Types

appEvent :: BrickEvent Name e -> EventM Name St ()
appEvent e@(VtyEvent ve) = case ve of
  EvKey KBackTab _     -> focusEvent F.focusPrev
  EvKey (KChar '\t') _ -> focusEvent F.focusNext
  EvKey KEnter _       -> openLink
  _                    -> mainViewportEvent e
appEvent e = resizeOrQuit e

focusEvent :: (FocusRing Name -> FocusRing Name)
           -> EventM Name St ()
focusEvent setter =
  modify $ \st -> st { focus = setter (focus st) }

openLink :: EventM Name St ()
openLink = do
  current <- F.focusGetCurrent <$> gets focus
  flip (maybe $ return ()) current $ \n -> do
    ls <- gets links
    maybe (return ()) linkHandler $
      find ((== n) . linkName) ls

mainViewportEvent :: BrickEvent Name e -> EventM Name St ()
mainViewportEvent e@(VtyEvent ve) = case ve of
  EvKey KUp _ -> vScrollBy vp (-1)
  EvKey KDown _ -> vScrollBy vp 1
  EvKey KPageUp _ -> vScrollBy vp (-10) --todo: more sensible length
  EvKey KPageDown _ -> vScrollBy vp 10  --same
  EvKey KHome _ -> vScrollToBeginning vp
  EvKey KEnd _ -> vScrollToEnd vp
  _ -> resizeOrQuit e
  where
    vp = viewportScroll MainViewport
mainViewportEvent e = resizeOrQuit e

openHome :: EventM Name St ()
openHome = do
  artcls <- getHomeArticles
  tgs <- getAllTags
  updateLinks [] -- TODO: expand with feed links
  modify $ \s -> s { currentPage = HomePage
                   , homeArticles = artcls
                   , allTags = tgs
                   }

openArticle :: ByteString -> EventM Name St ()
openArticle slug = do
  artcl <- getArticle slug
  updateLinks []
  modify $ \s -> s { currentPage = ArticlePage
                   , articleCurrent = artcl
                   }

-- For development
openNotImplemented :: EventM Name St ()
openNotImplemented = do
  updateLinks []
  modify $ \s -> s { currentPage = NotImplementedPage }

-- EventM actions to get content

getHomeArticles :: EventM Name St [Article]
getHomeArticles = do
  offset <- homeArticleOffset <$> get
  articles' <- request $
               requestArticleList [("limit", Just "10"),
                                   ("offset", Just $ offset)]
  return $ either (const []) articles articles'

getAllTags :: EventM Name St [Text]
getAllTags = do
  tags' <- request requestTags
  return $ either (const []) tags tags'

getArticle :: ByteString -> EventM Name St (Maybe Article)
getArticle slug = do
  artcl <- request $ requestArticle slug
  return $ either (const Nothing) (Just . article) artcl

request :: ConduitRequest (ConduitResponse a)
        -> EventM Name St (ConduitResponse a)
request rqst = do
  env' <- gets env
  liftIO $ runConduitRequest env' rqst

-- Standard layout links and event handler

updateLinks :: [Link] -> EventM Name St ()
updateLinks ls = modify $ updateStLinks $ mkAllLinksList ls

mkAllLinksList :: [Link] -> [Link]
mkAllLinksList ls = navConduitLink
                  : navHomeLink
                  : navSignInLink
                  : navSignUpLink
                  : ls
                 <> [footerConduitLink]                  

footerConduitLink,
  navConduitLink,
  navHomeLink,
  navSignInLink,
  navSignUpLink
  :: Link
footerConduitLink = Link FooterConduit openHome "conduit"
navConduitLink = Link NavConduit openHome "conduit"
navHomeLink = Link NavHome openHome "Home"
navSignInLink = Link NavSignIn openNotImplemented "Sign in"
navSignUpLink = Link NavSignUp openNotImplemented "Sign up"
