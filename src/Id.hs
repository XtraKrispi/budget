module Id where

import Data.Aeson
import Data.Text.Lazy qualified as LT
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import MyUUID (MyUUID)
import MyUUID qualified
import Relude
import Text.Read (Read (readsPrec))
import Web.Scotty.Trans (Parsable (..))

newtype Id a = Id {unId :: MyUUID}
  deriving (Show, Eq, ToField, FromField, FromJSON, ToJSON)

instance Read (Id a) where
  readsPrec _ input = case MyUUID.fromText (Relude.toText input) of
    Just uuid -> [(Id uuid, "")]
    Nothing -> []

instance Parsable (Id b) where
  parseParam :: forall k (a :: k). LT.Text -> Either LT.Text (Id a)
  parseParam txt = case MyUUID.fromText (LT.toStrict txt) of
    Just uuid -> Right (Id uuid)
    Nothing -> Left "Invalid id"

newId :: (MonadIO m) => m (Id a)
newId = Id <$> MyUUID.nextRandom

toText :: Id a -> Text
toText = MyUUID.toText . unId

fromText :: Text -> Maybe (Id a)
fromText = fmap Id . MyUUID.fromText