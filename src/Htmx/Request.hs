module Htmx.Request where

import Relude
import Web.Scotty.Trans (ActionT, header)

isHtmx :: (MonadIO m) => ActionT m Bool
isHtmx = isJust <$> header "HX-Request"

isBoosted :: (MonadIO m) => ActionT m Bool
isBoosted = isJust <$> header "HX-Boosted"