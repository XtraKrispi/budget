module Model.Email where

import Data.Text
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Handlers.Model

newtype Email = Email {unEmail :: Text}
  deriving (Show, Eq, Ord, FromField)

instance Parse Email where
  parse = pure . Email

instance ToField Email where
  toField :: Email -> SQLData
  toField (Email email) = SQLText (toLower email)