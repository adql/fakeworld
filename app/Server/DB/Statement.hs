{-# LANGUAGE QuasiQuotes #-}
module Server.DB.Statement
  ( selectArticle
  ) where

import Data.Text (Text)
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
