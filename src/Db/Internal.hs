module Db.Internal where

import AppError (AppError (..))
import Control.Arrow (ArrowChoice (left))
import Control.Exception (try)
import Control.Monad.Except
import Data.Text (pack)
import Data.Text.IO (hPutStrLn)
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Connection, setTrace, withConnection)
import Db (WithDb)
import Environment
import Relude

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
  (WithDb env m) =>
  (Connection -> IO a) ->
  m a
runDb cmd = do
  db <- asks dbPath
  env <- asks appEnvironment
  results <- liftIO $ fmap (left (\(e :: SomeException) -> DatabaseError $ pack $ show e)) $ try $ withTraceConnection db env cmd
  liftEither results
