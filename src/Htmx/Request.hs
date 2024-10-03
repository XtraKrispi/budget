module Htmx.Request where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isJust)
import Web.Scotty.Trans (ActionT, header)

isHtmx :: (MonadIO m) => ActionT m Bool
isHtmx = isJust <$> header "HX-Request"

isBoosted :: (MonadIO m) => ActionT m Bool
isBoosted = isJust <$> header "HX-Boosted"