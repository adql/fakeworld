module Env
  ( Env(..)
  ) where

import Data.ByteString (ByteString)

data Env = Env
  { requestHost :: ByteString
  , requestPort :: Int
  , requestSecure :: Bool
  } deriving Show
