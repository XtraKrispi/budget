module Htmx.Request where

import Data.Maybe (isJust)
import Web.Scotty.Trans (ActionT, header)

isHtmx :: (Monad m) => ActionT m Bool
isHtmx = isJust <$> header "HX-Request"

isBoosted :: (Monad m) => ActionT m Bool
isBoosted = isJust <$> header "HX-Boosted"