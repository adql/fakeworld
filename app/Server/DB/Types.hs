module Server.DB.Types
  ( ArticleRow
  , CommentRow
  , ProfileRow
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Time.Clock (UTCTime)
import Data.Int (Int16)

type ArticleRow =
  (Text, Text, Text, Text, Vector Text, UTCTime, UTCTime, Bool, Int16, Text, Maybe Text, Maybe Text, Bool)

type ProfileRow = (Text, Maybe Text, Maybe Text, Bool)

type CommentRow = (Int16, UTCTime, UTCTime, Text, Text, Maybe Text, Maybe Text, Bool)
