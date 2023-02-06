module Server.DB.Types
  ( ArticleRow
  , CommentRow
  , ProfileRow
  , ArticlesQuery
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Time.Clock (UTCTime)
import Data.Int (Int16)

type ArticleRow =
  (Text, Text, Text, Text, Vector Text, UTCTime, UTCTime, Bool, Int16, Text, Maybe Text, Maybe Text, Bool)

type ProfileRow = (Text, Maybe Text, Maybe Text, Bool)

type CommentRow = (Int16, UTCTime, UTCTime, Text, Text, Maybe Text, Maybe Text, Bool)

-- only includes tag, author and favorited; limit and offset performed
-- on the resulting vector in the Session
type ArticlesQuery = (Maybe Text, Maybe Text, Maybe Text)
