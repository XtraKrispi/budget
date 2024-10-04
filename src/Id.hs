module Id where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Text (Text)
import Data.Text.Lazy qualified as LT
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import MyUUID (MyUUID)
import MyUUID qualified
import Web.Scotty.Trans (Parsable (..))

newtype Id a = Id {unId :: MyUUID}
  deriving (Show, Eq, ToField, FromField, FromJSON, ToJSON)

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