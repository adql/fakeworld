{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.DummyDB
  ( dbQueryArticles
  , dummyTags
  , QueryArticles(..)
  ) where

-- Dummy database and access functions for development purposes

import Data.Function ((&))
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

applyQFilter :: Maybe a
             -> (a -> b -> Bool)
             -> [b] -> [b]
applyQFilter param toFilter =
  applyQParam param $ \p -> filter (toFilter p)

applyQParam :: Maybe a
            -> (a -> b -> b)
            -> b -> b
applyQParam param applyF =
  maybe (\x -> x) applyF param

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
                "Some dummy article"
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
                "Another dummy article"
                "How to do dummy stuff"
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

dummyComment :: Comment
dummyComment = Comment
               1
               dummyTime
               dummyTime
               "Spam comment"
               dummyProfile1

dummyTime :: UTCTime
dummyTime = UTCTime
            (fromOrdinalDate 2000 1)
            (secondsToDiffTime 0)
