module Model.Definition where

import Data.Text
import Data.Time (Day)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import GHC.Generics
import Handlers.Model (Parse (..))
import Model.Id (Id)
import Text.Read (readMaybe)

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
  fromField f =
    case fieldData f of
      SQLText "one-time" -> Ok OneTime
      SQLText "bi-weekly" -> Ok BiWeekly
      SQLText "monthly" -> Ok Monthly
      _ -> returnError Incompatible f "Unable to convert field to frequency"

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