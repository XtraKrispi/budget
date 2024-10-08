module Htmx.Request where

import Effects.WebServer
import Relude

isHtmx :: (MonadWebServer m) => m Bool
isHtmx = isJust <$> getRequestHeader "HX-Request"

isBoosted :: (MonadWebServer m) => m Bool
isBoosted = isJust <$> getRequestHeader "HX-Boosted"