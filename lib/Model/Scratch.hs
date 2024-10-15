module Model.Scratch where

import Data.Time (Day)
import Database.SQLite.Simple (FromRow)
import GHC.Generics (Generic)

data Scratch = Scratch
  { endDate :: Day
  , amountInBank :: Double
  , amountLeftOver :: Double
  }
  deriving (Show, Generic)

instance FromRow Scratch