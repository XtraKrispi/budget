module Model.Item where

import Data.Aeson (
  Encoding,
  FromJSON,
  Options (fieldLabelModifier),
  ToJSON (toEncoding),
  defaultOptions,
  genericToEncoding,
 )
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Model.Definition (Definition)
import Model.Id (Id)

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