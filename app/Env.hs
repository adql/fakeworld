{-# LANGUAGE OverloadedStrings #-}
module Env
  ( loadEnv
  , postgresSettings
  ) where

import qualified Data.ByteString.Char8 as BC
import qualified Hasql.Connection as Hasql
import Configuration.Dotenv.Environment (lookupEnv)
import Data.Maybe (fromJust)

import Configuration.Dotenv

loadEnv :: IO ()
loadEnv = withOnMissingFile $ loadFile $
          defaultConfig { configPath = [ ".env", ".env.defaults" ]
                        , configExamplePath = [".env.example"] }
  where
    withOnMissingFile = flip onMissingFile $ error "Missing .env file!"

postgresSettings :: IO Hasql.Settings
postgresSettings = do
  password <- BC.pack <$> unsafeLookupEnv "DB_PASSWORD"
  port <- read <$> unsafeLookupEnv "DB_PORT"
  return $ Hasql.settings "localhost" port "postgres" password "postgres"

-- For use if the existence of the variable is verified beforehand
unsafeLookupEnv :: String -> IO String
unsafeLookupEnv = fmap fromJust . lookupEnv
