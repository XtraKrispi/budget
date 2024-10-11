module Id where

import Data.Aeson
import Data.Text (Text)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Effectful
import Effects.MakeMyUUID (MakeMyUUID, generate)
import Handlers.Model (Parse (..))
import MyUUID (MyUUID)
import MyUUID qualified

newtype Id a = Id {unId :: MyUUID}
  deriving (Show, Eq, ToField, FromField, FromJSON, ToJSON)

instance Parse (Id b) where
  parse :: forall k (a :: k). Text -> Maybe (Id a)
  parse txt = Id <$> MyUUID.fromText txt

newId :: (MakeMyUUID :> es) => Eff es (Id a)
newId = Id <$> generate

toText :: Id a -> Text
toText = MyUUID.toText . unId

fromText :: Text -> Maybe (Id a)
fromText = fmap Id . MyUUID.fromText