module Main where

import Brick
import Control.Concurrent.Async (concurrently)
import Network.Wai.Handler.Warp

import Env
import qualified Env.Defaults
import Server
import TUI

main :: IO ()
main = runApp True

runApp :: Bool -> IO ()
runApp local = do
  let env = if local
            then Env.Defaults.conduitLocalAPI
            else Env.Defaults.conduitDemoAPI
  _ <- concurrently (defaultMain tui $ initialSt env)
                    (if local then runServer env else return ())
  return ()

runTUI :: Env -> IO ()
runTUI env = do
  _ <- defaultMain tui $ initialSt env
  return ()

runServer :: Env -> IO ()
runServer env = run (requestPort env) app
