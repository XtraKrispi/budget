module Model.MyUUID where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID4
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Effectful

newtype MyUUID = MyUUID {toUUID :: UUID.UUID}
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

instance ToField MyUUID where
  toField :: MyUUID -> SQLData
  toField (MyUUID uuid) = SQLText $ UUID.toText uuid

instance FromField MyUUID where
  fromField :: FieldParser MyUUID
  fromField f =
    case fieldData f of
      SQLText txt ->
        case UUID.fromText txt of
          Just uuid -> Ok (MyUUID uuid)
          _ -> returnError Incompatible f "Unable to convert to UUID"
      _ -> returnError Incompatible f "Not a text value"

nextRandom :: (MonadIO m) => m MyUUID
nextRandom = liftIO $ MyUUID <$> UUID4.nextRandom

toText :: MyUUID -> Text
toText = UUID.toText . toUUID

fromText :: Text -> Maybe MyUUID
fromText = fmap MyUUID . UUID.fromText