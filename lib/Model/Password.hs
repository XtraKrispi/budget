module Model.Password where

import Data.Text (Text)
import Database.SQLite.Simple (
  ResultError (Incompatible),
  SQLData (SQLText),
 )
import Database.SQLite.Simple.FromField (
  FieldParser,
  FromField (..),
  fieldData,
  returnError,
 )
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Handlers.Model (Parse (..))
import Model.Common (Hashed, PlainText)

newtype Password a = Password {unPassword :: Text}
  deriving (Show, Eq)

instance Parse (Password PlainText) where
  parse :: Text -> Maybe (Password PlainText)
  parse = pure . Password
instance ToField (Password Hashed) where
  toField :: Password Hashed -> SQLData
  toField = SQLText . unPassword

instance FromField (Password Hashed) where
  fromField :: FieldParser (Password Hashed)
  fromField f =
    case fieldData f of
      SQLText txt -> Ok (Password txt)
      _ -> returnError Incompatible f "Unable to convert to proper password hash"
