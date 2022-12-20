{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.DummyDB
  ( dbQueryArticle
  , dbQueryArticles
  , dbQueryComments
  , dbQueryProfile
  , dummyTags
  , QueryArticles(..)
  ) where

-- Dummy database and access functions for development purposes

import Control.Monad.Except (catchError)
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Servant

import API.Response.Types 

data QueryArticles =
  QueryArticles { quTagged :: Maybe Tag
                , quByAuthor :: Maybe Text
                , quFavoritedBy :: Maybe Text
                , quLimit :: Maybe Int
                , quOffset :: Maybe Int
                }

dbQueryArticles :: QueryArticles -> Handler Articles
dbQueryArticles (QueryArticles {..}) = return $
                       dummyArticles { articles = foldl (&)
                                                  (articles dummyArticles)
                                                  [q1, q2, q3, q4, q5] }
  where
    q1 = applyQFilter quTagged $ \p -> elem p . articleTagList
    q2 = applyQFilter quByAuthor $ \p -> (== p) . profileUsername . articleAuthor
    q3 = applyQFilter quFavoritedBy $ \_ -> const True --unimplemented
    q4 = applyQParam quOffset $ \p -> drop p
    q5 = applyQParam quLimit $ \p -> take p

dbQueryArticle :: Text -> Handler Article'
dbQueryArticle slug = 
  let art = find ((== slug) . articleSlug) (articles dummyArticles) in
    maybe (throwError $ notFoundError "article") (return . Article') art

dbQueryComments :: Text -> Handler Comments
dbQueryComments slug =
  (Comments . fromJust . flip lookup dummyCommentTable . article) <$>
  dbQueryArticle slug

dbQueryProfile :: Text -> Handler Profile'
dbQueryProfile username =
  let pr = find ((== username) . profileUsername) dummyProfiles in
    maybe (throwError $ notFoundError "profile") (return . Profile') pr

notFoundError :: BSL.ByteString -> ServerError
notFoundError q = jsonError $
                  err404 { errBody = "{\"errors\":{\"" <> q <> "\":[\"not found\"]}}" }

jsonError :: ServerError -> ServerError
jsonError e = e { errHeaders = ("Content-Type", "application/json;charset=utf-8") : errHeaders e }

applyQFilter :: Maybe a
             -> (a -> b -> Bool)
             -> [b] -> [b]
applyQFilter param toFilter =
  applyQParam param $ \p -> filter (toFilter p)

applyQParam :: Maybe a
            -> (a -> b -> b)
            -> b -> b
applyQParam param applyF =
  maybe id applyF param

dummyProfile1 :: Profile
dummyProfile1 = Profile "Dummy Profile 1"
               (Just "Some random dummy profile")
               Nothing
               True

dummyProfile2 :: Profile
dummyProfile2 = Profile "Dummy Profile 2"
               (Just "Some other random dummy profile")
               Nothing
               True

dummyProfiles :: [Profile]
dummyProfiles = [dummyProfile1, dummyProfile2]

dummyUser :: User
dummyUser = User
            "dummy@mum.my"
            "SomeTokenThatVerifiesNothing"
            "dummy"
            (Just "Some random dummy profile")
            Nothing

dummyTags :: Tags
dummyTags = Tags ["dummy-1", "dummy-2"]

dummyArticle1 :: Article
dummyArticle1 = Article
                "How-to-do-dummy-stuff"
                "How to do dummy stuff"
                "Useless dummy information"
                "Lorem ipsum dummy ipsum dummy"
                [ tags dummyTags !! 0 ]
                dummyTime
                dummyTime
                True
                1
                dummyProfile1

dummyArticle2 :: Article
dummyArticle2 = Article
                "How-to-do-more-dummy-stuff"
                "How to do more dummy stuff"
                "Useless dummy information"
                "Lorem ipsum dummy ipsum dummy"
                [ tags dummyTags !! 1 ]
                dummyTime
                dummyTime
                True
                1
                dummyProfile2

dummyArticles :: Articles
dummyArticles = Articles [dummyArticle1, dummyArticle2] 2

dummyComment1 :: Comment
dummyComment1 = Comment
               1
               dummyTime
               dummyTime
               "Spam comment"
               dummyProfile1

dummyComment2 :: Comment
dummyComment2 = Comment
               2
               dummyTime
               dummyTime
               "Another spam comment"
               dummyProfile2

dummyCommentTable :: [(Article, [Comment])]
dummyCommentTable = [ (dummyArticle1, [dummyComment1])
                    , (dummyArticle2, [dummyComment2])
                    ]

dummyTime :: UTCTime
dummyTime = UTCTime
            (fromOrdinalDate 2000 1)
            (secondsToDiffTime 0)
