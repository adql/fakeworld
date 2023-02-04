{-# LANGUAGE QuasiQuotes #-}
module Server.DB.Statement
  ( selectArticle
  , selectComments
  , selectProfile
  , selectAllTags
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Statement
import Hasql.TH

import Server.DB.Types

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
