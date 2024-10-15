module Htmx.Request where

import Data.Foldable (find)
import Data.Maybe (isJust)
import Handlers.Model (Request (..))

isHtmx :: Request -> Bool
isHtmx request = isJust $ find (\(t, _) -> t == "HX-Request") request.requestHeaders

isBoosted :: Request -> Bool
isBoosted request = isJust $ find (\(t, _) -> t == "HX-Boosted") request.requestHeaders