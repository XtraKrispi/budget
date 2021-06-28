module Application.Helper.View where

import qualified Data.Text as T
import Data.Text.Format.Numbers
import Data.Time.Format.ISO8601 (iso8601Show)
import Generated.Types
import IHP.ViewPrelude

-- Here you can add functions which are available in all your views

prettyFreq :: Frequency -> Text
prettyFreq Onetime = "One Time"
prettyFreq Weekly = "Weekly"
prettyFreq Biweekly = "Bi-Weekly"
prettyFreq Monthly = "Monthly"

instance CanSelect AmountType where
  type SelectValue AmountType = AmountType
  selectValue value = value

  selectLabel Debit = "Debit"
  selectLabel Credit = "Credit"

instance CanSelect Frequency where
  type SelectValue Frequency = Frequency
  selectValue value = value

  selectLabel = prettyFreq

isoDate :: Day -> Text
isoDate = T.pack . iso8601Show

formatCurrency :: Double -> Text
formatCurrency = ("$" <>) . prettyF (PrettyCfg 2 (Just ',') '.')