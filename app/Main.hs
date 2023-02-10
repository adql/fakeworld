{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Brick
import Control.Concurrent.Async (concurrently)
import Network.Wai.Handler.Warp
import Options.Applicative (execParser,)
import Servant.Client (BaseUrl, baseUrlPort)

import qualified Env.Defaults
import Options
import Server
import TUI

main :: IO ()
main = do
  options <- execParser opts
  runApp options

runApp :: Options -> IO ()
runApp (Options {external, serverOnly, dark}) = do
  let ext = if serverOnly then False else external
      baseUrl = if ext
                then Env.Defaults.conduitDemoBaseUrl
                else Env.Defaults.conduitLocalBaseUrl
  _ <- concurrently (if serverOnly then return () else runTUI baseUrl dark)
                    (if ext then return () else runServer baseUrl)
  return ()

runTUI :: BaseUrl -> Bool -> IO ()
runTUI baseUrl dark = do
  _ <- defaultMain tui $ initialSt baseUrl dark
  return ()

runServer :: BaseUrl -> IO ()
runServer baseUrl = run (baseUrlPort baseUrl) app
