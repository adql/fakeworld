module Server.DB
  ( module Server.DB.Session
  , dbGet
  , dbGetMaybe
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import Hasql.Session (Session)
import qualified Hasql.Connection as Hasql (acquire)
import qualified Hasql.Session as Hasql (run)
import Servant.Server (Handler)
import qualified Servant.Server as Servant

import Env (postgresSettings)
import Server.DB.Session

dbGet :: Session a -> Handler a
dbGet = dbQuery

dbGetMaybe :: Session (Maybe a) -> Handler a
dbGetMaybe session = dbQuery session >>=
  maybe (throwError Servant.err404) return

dbQuery :: Session a -> Handler a
dbQuery session = do
  connection' <- liftIO $ Hasql.acquire =<< postgresSettings
  case connection' of
    Left _cError -> throwError Servant.err500
    Right connection -> do
      result <- liftIO $ Hasql.run session connection
      either (const (throwError Servant.err500)) return result
