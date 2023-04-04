module API.Common
  ( withPrefixRemoval
  , asObjectWithSingleField
  ) where

import Data.Aeson
import Data.Char (toLower)
import Data.List (uncons)

withPrefixRemoval :: Int -> Options -> Options
withPrefixRemoval len ops = ops { fieldLabelModifier = unPrefix }
  where
    unPrefix =
      maybe "" (\(c,cs) -> toLower c : cs) . uncons . drop len

asObjectWithSingleField :: String -> Options
asObjectWithSingleField key =
  defaultOptions { sumEncoding = ObjectWithSingleField
                 , constructorTagModifier = const key
                 , tagSingleConstructors = True
                 }

