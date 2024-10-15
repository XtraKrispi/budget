module Db.Init where

import AppError (AppError)
import Data.Foldable (traverse_)
import Database.SQLite.Simple (execute_)
import Db.Internal
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Environment

initialize ::
  ( IOE :> es
  , Reader Environment :> es
  , Error AppError :> es
  ) =>
  Eff es ()
initialize =
  runDb \conn -> do
    let statements =
          [ "CREATE TABLE IF NOT EXISTS definitions(id INTEGER PRIMARY KEY AUTOINCREMENT, identifier TEXT, description TEXT, amount REAL, frequency TEXT, start_date TEXT, end_date TEXT NULL, is_automatic_withdrawal INTEGER, user_id INTEGER);"
          , "CREATE TABLE IF NOT EXISTS archive(id INTEGER PRIMARY KEY AUTOINCREMENT, identifier TEXT, item_definition_identifier TEXT, description TEXT, amount REAL, date TEXT, action_date TEXT, action TEXT, user_id INTEGER);"
          , "CREATE TABLE IF NOT EXISTS users(id INTEGER PRIMARY KEY AUTOINCREMENT, email TEXT, password_hash TEXT, name TEXT);"
          , "CREATE TABLE IF NOT EXISTS sessions(session_id TEXT PRIMARY KEY, user_id INTEGER, expiration_time TEXT);"
          , "CREATE TABLE IF NOT EXISTS scratch(id INTEGER PRIMARY KEY AUTOINCREMENT, user_id INTEGER UNIQUE, end_date TEXT, amount_in_bank REAL, amount_left_over REAL);"
          , "CREATE TABLE IF NOT EXISTS password_reset_tokens (user_id TEXT NOT NULL, token TEXT NOT NULL UNIQUE, token_expiry TEXT NOT NULL, PRIMARY KEY (user_id, token));"
          ]
    traverse_ (execute_ conn) statements
