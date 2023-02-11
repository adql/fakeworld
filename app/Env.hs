{-# LANGUAGE OverloadedStrings #-}
module Env
  ( loadEnv
  ) where

import Configuration.Dotenv

loadEnv :: IO ()
loadEnv = withOnMissingFile $ loadFile $
          defaultConfig { configPath = [ ".env", ".env.defaults" ]
                        , configExamplePath = [".env.example"] }
  where
    withOnMissingFile = flip onMissingFile $ error "Missing .env file!"
