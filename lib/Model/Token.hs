module Model.Token where

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

newtype Token a = Token {unToken :: Text}
  deriving (Show, Eq)

instance ToField (Token Hashed) where
  toField :: Token Hashed -> SQLData
  toField (Token token) = SQLText token

instance FromField (Token Hashed) where
  fromField :: FieldParser (Token Hashed)
  fromField f =
    case fieldData f of
      SQLText txt -> Ok (Token txt)
      _ -> returnError Incompatible f "Unable to convert to proper token"

instance Parse (Token PlainText) where
  parse :: Text -> Maybe (Token PlainText)
  parse = pure . Token