module Model where

import Data.Aeson (Encoding, FromJSON, ToJSON (toEncoding), defaultOptions, fieldLabelModifier, genericToEncoding)
import Data.Text (toLower)
import Data.Text.Lazy qualified as LT
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Database.SQLite.Simple (SQLData (..))
import Database.SQLite.Simple.FromField (FieldParser, FromField (..), ResultError (..), fieldData, returnError)
import Database.SQLite.Simple.FromRow (FromRow (..))
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import Id
import MyUUID
import Relude
import Text.Read (readMaybe)
import Web.Scotty (Parsable (..))

data PlainText
data Hashed

newtype Password a = Password {unPassword :: Text}
  deriving (Show, Eq)

instance ToField (Password Hashed) where
  toField :: Password Hashed -> SQLData
  toField (Password pass) = SQLText pass

instance FromField (Password Hashed) where
  fromField :: FieldParser (Password Hashed)
  fromField f =
    case fieldData f of
      SQLText txt -> Ok (Password txt)
      _ -> returnError Incompatible f "Unable to convert to proper password hash"

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

instance Parsable (Token PlainText) where
  parseParam :: LT.Text -> Either LT.Text (Token PlainText)
  parseParam = pure . Token . LT.toStrict

instance Parsable (Password PlainText) where
  parseParam :: LT.Text -> Either LT.Text (Password PlainText)
  parseParam = pure . Password . LT.toStrict

newtype Email = Email {unEmail :: Text}
  deriving (Show, FromField, Parsable)

instance ToField Email where
  toField :: Email -> SQLData
  toField (Email email) = SQLText (toLower email)

data User = User
  { email :: Email
  , name :: Text
  , passwordHash :: Password Hashed
  }
  deriving (Generic)
instance FromRow User

data AlertType
  = Info
  | Success
  | Warning
  | Error
  deriving (Eq)

newtype SessionId = SessionId {unSessionId :: MyUUID}
  deriving (Eq, ToField, FromField)

data Frequency
  = OneTime
  | BiWeekly
  | Monthly
  deriving (Eq, Enum, Bounded, Show, Read)

instance Parsable Frequency where
  parseParam :: LT.Text -> Either LT.Text Frequency
  parseParam p =
    case readMaybe (LT.unpack p) of
      Just f -> Right f
      Nothing -> Left "There was a problem parsing the frequency"

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

instance Parsable MyDay where
  parseParam :: LT.Text -> Either LT.Text MyDay
  parseParam date =
    case iso8601ParseM (LT.unpack date) of
      Just d -> Right (MyDay d)
      Nothing -> Left "Invalid date"