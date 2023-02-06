{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Server.DB.Statement
  ( selectArticles
  , selectArticle
  , selectComments
  , selectProfile
  , selectAllTags
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Decoders as D
import Hasql.DynamicStatements.Snippet (param)
import Hasql.DynamicStatements.Statement
import Hasql.Statement
import Hasql.TH

import Server.DB.Types

-- todo: query with composite Postgres data types to reduce
-- boilerplate

selectArticles :: ArticlesQuery -> Statement () (Vector ArticleRow)
selectArticles (tag, author, _favorited) =
  --query by favorited is not yet implemented
  dynamicallyParameterized snippet decoder True
  where
    snippet =
      "SELECT slug, \
              \title, \
              \description, \
              \body, \
              \tag_list, \
              \created_at, \
              \updated_at, \
              \false, \
              \0, \
              \username, \
              \bio, \
              \image, \
              \false \
       \FROM article a JOIN profile p \
         \ON a.author_id = p.profile_id "
       <> "WHERE true " --ugly way to allow an empty WHERE clause
         <> foldMap (\x -> "AND " <> param x <> " = ANY (tag_list) ") tag
         <> foldMap (\x -> "AND username = " <> param x) author
    decoder = D.rowVector $ (,,,,,,,,,,,,)
      <$> D.column (D.nonNullable D.text)
      <*> D.column (D.nonNullable D.text)
      <*> D.column (D.nonNullable D.text)
      <*> D.column (D.nonNullable D.text)
      <*> D.column (D.nonNullable $ D.vectorArray $ D.nonNullable D.text)
      <*> D.column (D.nonNullable D.timestamptz)
      <*> D.column (D.nonNullable D.timestamptz)
      <*> D.column (D.nonNullable D.bool)
      <*> D.column (D.nonNullable D.int2)
      <*> D.column (D.nonNullable D.text)
      <*> D.column (D.nullable D.text)
      <*> D.column (D.nullable D.text)
      <*> D.column (D.nonNullable D.bool)
      
selectArticle :: Statement Text (Maybe ArticleRow)
selectArticle = --favorited, favoritesCount and (profile) following not yet implemented
  [maybeStatement|
     SELECT slug :: text,
            title :: text,
            description :: text,
            body :: text,
            tag_list :: text[],
            created_at :: timestamptz,
            updated_at :: timestamptz,
            false :: bool,
            0 :: int2,
            username :: text,
            bio :: text?,
            image :: text?,
            false :: bool
     FROM article a JOIN profile p
       ON a.author_id = p.profile_id
     WHERE slug = $1 :: text
     |]

selectComments :: Statement Text (Vector CommentRow)
selectComments = --(profile) following is not yet implemented
  [vectorStatement|
     SELECT comment_id :: int2,
            created_at :: timestamptz,
            updated_at :: timestamptz,
            body :: text,
            username :: text,
            bio :: text?,
            image :: text?,
            false :: bool
     FROM comment c JOIN profile p
       ON c.author_id = p.profile_id
     WHERE c.article_id IN
       (SELECT article_id FROM article
        WHERE slug = $1 :: text)
        |]

selectProfile :: Statement Text (Maybe ProfileRow)
selectProfile = --following is not yet implemented
  [maybeStatement|
     SELECT username :: text,
            bio :: text?,
            image :: text?,
            false :: bool
     FROM profile
     WHERE username = $1 :: text
     |]

selectAllTags :: Statement () (Vector Text)
selectAllTags =
  [vectorStatement|
     SELECT DISTINCT unnest(tag_list) :: text
     FROM article
     |]
