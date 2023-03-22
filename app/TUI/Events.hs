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
import Data.List (find)
import Data.Text (Text)
import Graphics.Vty.Input.Events (Event(..), Key(..))
import Network.HTTP.Client.Conduit (newManager)
import Servant.Client

import qualified API.Request as API
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
  modify $ \st -> st { stFocus = setter (stFocus st) }

openLink :: EventM Name St ()
openLink = do
  current <- F.focusGetCurrent <$> gets stFocus
  maybe' current $ \name -> do
    l' <- find ((== name) . linkName) <$> gets stLinks
    maybe' l' $ \l ->
      vScrollToBeginning (viewportScroll MainViewport)
      >> linkHandler l
  where
    maybe' = flip (maybe $ return ())

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
  articles' <- getHomeArticles
  tgs <- getAllTags
  updateLinks $ mkFeedLinks articles'
  modify $ \s -> s { stCurrentPage = HomePage
                   , stArticles = articles'
                   , stAllTags = tgs
                   }

openArticle :: Text -> EventM Name St ()
openArticle slug = do
  article' <- getArticle slug
  updateLinks []
  modify $ \s -> s { stCurrentPage = ArticlePage
                   , stArticleCurrent = article'
                   }

-- For development
openNotImplemented :: EventM Name St ()
openNotImplemented = do
  updateLinks []
  modify $ \s -> s { stCurrentPage = NotImplementedPage }

mkFeedLinks :: [Article] -> [Link]
mkFeedLinks articles' = flip map articles' $ \article' ->
  let slug = articleSlug article' in
    Link { linkName = LinkName slug
         , linkHandler = openArticle slug
         , linkText = show $ articleTitle article'
         }

-- EventM actions to get content

getHomeArticles :: EventM Name St [Article]
getHomeArticles = do
  offset <- gets stFilterOffset
  articles' <- request $
               API.listArticles Nothing Nothing Nothing (Just 10) (Just offset)
  return $ either (const []) articles articles'

getAllTags :: EventM Name St [Text]
getAllTags = do
  allTags <- request API.getTags
  return $ either (const []) tags allTags

getArticle :: Text -> EventM Name St (Maybe Article)
getArticle slug = do
  article' <- request $ API.getArticle slug
  return $ either (const Nothing) (Just . article) article'

request :: ClientM a -> EventM Name St (Either ClientError a)
request rqst = do
  baseUrl' <- gets stBaseUrl
  liftIO $ do
    manager' <- newManager
    runClientM rqst
      (mkClientEnv manager' baseUrl')

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
