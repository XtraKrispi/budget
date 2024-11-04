module Htmx.Request where

import Data.Foldable (find)
import Data.Maybe (isJust)
import Data.Text (toLower)
import Handlers.Model (Request (..))

isHtmx :: Request -> Bool
isHtmx request = isJust $ find (\(t, _) -> toLower t == "hx-request") request.requestHeaders

isBoosted :: Request -> Bool
isBoosted request = isJust $ find (\(t, _) -> toLower t == "hx-boosted") request.requestHeaders