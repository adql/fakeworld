module TUI.Events
  ( appEvent
  , mainViewportEvent
  , openArticle
  , openHomeGlobal
  , openHomeTag
  , openNotImplemented
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

openHomeGlobal :: EventM Name St ()
openHomeGlobal = filterReset >> openHome

openHomeTag :: Tag -> EventM Name St ()
openHomeTag tag = filterApplyTag tag >> openHome

openHome :: EventM Name St ()
openHome = do
  articles' <- getHomeArticles
  tgs <- getAllTags
  feedNavGlobalFeedLink <- gets mkFeedNavGlobalFeedLink
  let pageLinks = concat [ feedNavGlobalFeedLink
                         , mkTagLinks tgs
                         , mkFeedLinks articles'
                         ]
  updateLinks pageLinks
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

mkFeedLinks :: [Article] -> [Link]
mkFeedLinks = map $ \article' ->
  let slug = articleSlug article' in
    Link (LinkName slug) (openArticle slug)
  
mkFeedNavGlobalFeedLink :: St -> [Link]
mkFeedNavGlobalFeedLink = maybe []
                          (\_ -> [Link FeedNavGlobalFeed openHomeGlobal]) .
                          stFilterTag

mkTagLinks :: [Tag] -> [Link]
mkTagLinks = map $ \tag ->
  Link (LinkName tag) (openHomeTag tag)

filterApplyTag :: Tag -> EventM Name St ()
filterApplyTag tag = filterApply (Just tag) Nothing Nothing Nothing

filterReset :: EventM Name St ()
filterReset = filterApply Nothing Nothing Nothing Nothing

filterApply :: Maybe Tag ->
               Maybe Text -> --to be implemented
               Maybe Text -> --to be implemented
               Maybe Int ->
               EventM Name St ()
filterApply tag _author _user offset =
  modify $ \s -> s { stFilterOffset = maybe 0 id offset
                   , stFilterTag = tag
                   }

-- EventM actions to get content

getHomeArticles :: EventM Name St [Article]
getHomeArticles = do
  offset <- gets stFilterOffset
  tag <- gets stFilterTag
  articles' <- request $
               API.listArticles tag Nothing Nothing (Just 10) (Just offset)
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
footerConduitLink = Link FooterConduit openHomeGlobal
navConduitLink = Link NavConduit openHomeGlobal
navHomeLink = Link NavHome openHomeGlobal
navSignInLink = Link NavSignIn openNotImplemented
navSignUpLink = Link NavSignUp openNotImplemented

-- For development --

withLocallyNotImplemented :: EventM Name St () -> EventM Name St ()
withLocallyNotImplemented e = do
  BaseUrl _ host _ _ <- gets stBaseUrl
  if host == "localhost"
    then openLocallyNotImplemented
    else e

openNotImplemented ::EventM Name St ()
openNotImplemented = openNotImplemented' "This is a work in progress..."

openLocallyNotImplemented ::EventM Name St ()
openLocallyNotImplemented =
  openNotImplemented' "This section is only implemented on the front-end. \
                      \Use \"make run-external\" to run the app with external API."

openNotImplemented' :: String -> EventM Name St ()
openNotImplemented' msg = do
  updateLinks []
  modify $ \s -> s { stCurrentPage = NotImplementedPage msg}
