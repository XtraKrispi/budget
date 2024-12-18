module Db.Internal where

import AppError (AppError (DatabaseError))
import Data.Text (pack)
import Data.Text.IO (hPutStrLn)
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Connection, setTrace, withConnection)
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader, asks)
import Environment
import Interpreters.Helpers (adapt)
import System.IO (stderr)

withTraceConnection ::
  DbPath ->
  Env ->
  (Connection -> IO a) ->
  IO a
withTraceConnection (DbPath dbPath') appEnv action = do
  withConnection dbPath' \conn -> do
    case appEnv of
      Dev -> do
        now <- getCurrentTime
        setTrace conn $ Just $ \t -> hPutStrLn stderr $ "SQLITE @ " <> pack (show now) <> ":  " <> t
      Prod -> setTrace conn Nothing
    action conn

runDb ::
  (IOE :> es, Reader Environment :> es, Error AppError :> es) =>
  (Connection -> IO a) ->
  Eff es a
runDb cmd = do
  db <- asks envDbPath
  env <- asks envAppEnvironment
  adapt DatabaseError $ withTraceConnection db env cmd
