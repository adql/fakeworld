module Options
  ( Options(..)
  , opts
  , optsParser
  ) where

import Options.Applicative

data Options = Options
  { external :: Bool
  , serverOnly :: Bool
  , dark :: Bool
  }

optsParser :: Parser Options
optsParser = Options
      <$> switch ( long "external-api"
                   <> short 'e'
                   <> help "Whether to use api.realworld.io instead of local api" )
      <*> switch ( long "server-only"
                   <> short 's'
                   <> help "Only run the server. Excludes --external-api")
      <*> switch ( long "dark-mode"
                   <> short 'd'
                   <> help "Start the application in dark mode"
                 )

opts :: ParserInfo Options
opts = info (optsParser <**> helper)
  ( fullDesc
    <> progDesc "Conduit (RealWorld) TUI frontend and backend" )
