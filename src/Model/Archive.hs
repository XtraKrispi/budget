module Model.Archive where

import Data.Text
import Data.Time (Day)
import Database.SQLite.Simple (FromRow, ResultError (..), SQLData (..))
import Database.SQLite.Simple.FromField (FieldParser, FromField (fromField), fieldData, returnError)
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import Model.Definition (Definition)
import Model.Id (Id)

data ArchiveAction = Paid | Skipped
  deriving (Show)

instance FromField ArchiveAction where
  fromField :: FieldParser ArchiveAction
  fromField field =
    case fieldData field of
      SQLText "paid" -> Ok Paid
      SQLText "skipped" -> Ok Skipped
      _ -> returnError Incompatible field "Unable to convert field to archive action"

instance ToField ArchiveAction where
  toField :: ArchiveAction -> SQLData
  toField Paid = SQLText "paid"
  toField Skipped = SQLText "skipped"

data ArchivedItem = ArchivedItem
  { archivedItemId :: Id ArchivedItem
  , archivedItemItemDefinitionId :: Id Definition
  , archivedItemDescription :: Text
  , archivedItemAmount :: Double
  , archivedItemDate :: Day
  , archivedItemActionDate :: Day
  , archivedItemAction :: ArchiveAction
  }
  deriving (Show, Generic)

instance FromRow ArchivedItem