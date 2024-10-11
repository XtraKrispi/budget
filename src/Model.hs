module Model where

import Data.Aeson (Encoding, FromJSON, ToJSON (toEncoding), defaultOptions, fieldLabelModifier, genericToEncoding)
import Data.Text (Text, pack, toLower, unpack)
import Data.Time (Day, UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField (FieldParser, FromField (..), ResultError (..), fieldData, returnError)
import Database.SQLite.Simple.FromRow (FromRow (..))
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import Handlers.Model (Parse (..))
import Id
import MyUUID
import Text.Read

data PlainText
data Hashed

newtype Password a = Password {unPassword :: Text}
  deriving (Show, Eq)

instance Read (Password PlainText) where
  readsPrec _ input = [(Password (pack input), "")]

instance Parse (Password PlainText) where
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

newtype Token a = Token {unToken :: Text}
  deriving (Show, Eq)

instance Read (Token PlainText) where
  readsPrec _ input = [(Token (pack input), "")]

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

newtype Email = Email {unEmail :: Text}
  deriving (Show, Eq, Ord, FromField)

instance Read Email where
  readsPrec _ input = [(Email (pack input), "")]

instance Parse Email where
  parse = pure . Email

instance ToField Email where
  toField :: Email -> SQLData
  toField (Email email) = SQLText (toLower email)

data User = User
  { email :: Email
  , name :: Text
  , passwordHash :: Password Hashed
  }
  deriving (Generic, Show)
instance FromRow User

data AlertType
  = Info
  | Success
  | Warning
  | Error
  deriving (Eq)

newtype SessionId = SessionId {unSessionId :: MyUUID}
  deriving (Show, Eq, ToField, FromField)

data Frequency
  = OneTime
  | BiWeekly
  | Monthly
  deriving (Eq, Enum, Bounded, Show, Read)

instance Parse Frequency where
  parse :: Text -> Maybe Frequency
  parse = readMaybe . unpack

instance FromField Frequency where
  fromField :: FieldParser Frequency
  fromField field =
    case fieldData field of
      SQLText "one-time" -> Ok OneTime
      SQLText "bi-weekly" -> Ok BiWeekly
      SQLText "monthly" -> Ok Monthly
      _ -> returnError Incompatible field "Unable to convert field to frequency"

instance ToField Frequency where
  toField :: Frequency -> SQLData
  toField OneTime = SQLText "one-time"
  toField BiWeekly = SQLText "bi-weekly"
  toField Monthly = SQLText "monthly"

data Definition = Definition
  { definitionId :: Id Definition
  , definitionDescription :: Text
  , definitionAmount :: Double
  , definitionFrequency :: Frequency
  , definitionStartDate :: Day
  , definitionEndDate :: Maybe Day
  , definitionIsAutomaticWithdrawal :: Bool
  }
  deriving (Show, Generic)

instance FromRow Definition

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

data Scratch = Scratch
  { endDate :: Day
  , amountInBank :: Double
  , amountLeftOver :: Double
  }
  deriving (Show, Generic)

instance FromRow Scratch

data Item = Item
  { itemDefinitionId :: Id Definition
  , itemDescription :: Text
  , itemAmount :: Double
  , itemDate :: Day
  , itemIsAutomaticWithdrawal :: Bool
  }
  deriving (Generic)

instance ToJSON Item where
  toEncoding :: Item -> Encoding
  toEncoding = genericToEncoding defaultOptions{fieldLabelModifier = drop 4}

instance FromJSON Item

newtype MyDay = MyDay {unMyDay :: Day}
  deriving (Show)

instance Read MyDay where
  readsPrec :: Int -> ReadS MyDay
  readsPrec _ input = case iso8601ParseM input of
    Just d -> [(MyDay d, "")]
    Nothing -> []

instance Parse MyDay where
  parse :: Text -> Maybe MyDay
  parse date = MyDay <$> iso8601ParseM (unpack date)

newtype ExpirationTime = ExpirationTime {unExpirationTime :: UTCTime}
