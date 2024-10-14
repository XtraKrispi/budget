module Model.Id where

import Data.Aeson
import Data.Text (Text)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Handlers.Model (Parse (..))
import Model.MyUUID (MyUUID)
import Model.MyUUID qualified as MyUUID

newtype Id a = Id {unId :: MyUUID}
  deriving (Show, Eq, ToField, FromField, FromJSON, ToJSON)

instance Parse (Id b) where
  parse :: forall k (a :: k). Text -> Maybe (Id a)
  parse txt = Id <$> MyUUID.fromText txt

toText :: Id a -> Text
toText = MyUUID.toText . unId

fromText :: Text -> Maybe (Id a)
fromText = fmap Id . MyUUID.fromText