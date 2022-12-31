{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Brick
import Control.Concurrent.Async (concurrently)
import Network.Wai.Handler.Warp
import Options.Applicative

import Env
import qualified Env.Defaults
import Server
import TUI

main :: IO ()
main = do
  options <- execParser opts
  runApp options

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

runApp :: Options -> IO ()
runApp (Options {external, serverOnly, dark}) = do
  let ext = if serverOnly then False else external
      env = if ext
            then Env.Defaults.conduitDemoAPI
            else Env.Defaults.conduitLocalAPI
  _ <- concurrently (if serverOnly then return () else runTUI env dark)
                    (if ext then return () else runServer env)
  return ()

runTUI :: Env -> Bool -> IO ()
runTUI env dark = do
  _ <- defaultMain tui $ initialSt env dark
  return ()

runServer :: Env -> IO ()
runServer env = run (requestPort env) app
