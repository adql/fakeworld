{-# LANGUAGE RecordWildCards #-}
module Server.DB.Session
  ( getArticles
  , getArticle
  , getComments
  , getProfile
  , getAllTags
  ) where

import Data.Text (Text)
import qualified Data.Vector as V
import Hasql.Session

import API.Response.Types
import qualified Server.DB.Statement as S
import Server.DB.Types

getArticles :: ArticlesQuery -> Maybe Int -> Maybe Int -> Session Articles
getArticles query limit' offset' = do
  result <- statement () (S.selectArticles query)
  let limit  = maybe 20 id limit'
      offset = maybe 0 id offset'
      slice  = V.take limit $ V.drop offset result
  return $ Articles
    (map rowToArticle $ V.toList slice)
    (V.length result)

getArticle :: Text -> Session (Maybe Article')
getArticle slug = statement slug S.selectArticle >>=
  return . fmap (Article' . rowToArticle)

rowToArticle :: ArticleRow -> Article
rowToArticle ( articleSlug
             , articleTitle
             , articleDescription
             , articleBody
             , articleTagList
             , articleCreatedAt
             , articleUpdatedAt
             , articleFavorited
             , articleFavoritesCount
             , profileUsername
             , profileBio
             , profileImage
             , profileFollowing
             )
  = Article { articleFavoritesCount = fromIntegral articleFavoritesCount
            , articleTagList = V.toList articleTagList
            , articleAuthor = Profile {..}
            , ..
            }

getComments :: Text -> Session Comments
getComments slug = statement slug S.selectComments >>=
  return . Comments . map rowToComment . V.toList

rowToComment :: CommentRow -> Comment
rowToComment ( commentId
             , commentCreatedAt
             , commentUpdatedAt
             , commentBody
             , profileUsername
             , profileBio
             , profileImage
             , profileFollowing
             )
  = Comment { commentId = fromIntegral commentId
            , commentAuthor = Profile { .. }, .. }

getProfile :: Text -> Session (Maybe Profile')
getProfile username = statement username S.selectProfile >>=
  return . fmap (Profile' . rowToProfile)

rowToProfile :: ProfileRow -> Profile
rowToProfile ( profileUsername
             , profileBio
             , profileImage
             , profileFollowing
             )
  = Profile { .. }

getAllTags :: Session Tags
getAllTags = statement () S.selectAllTags >>=
  return . Tags . V.toList
