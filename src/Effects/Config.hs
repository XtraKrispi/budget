module Effects.Config where

import Environment (BaseUrl)
import Relude

class (Monad m) => MonadConfig m where
  getBaseUrl :: m BaseUrl

instance (MonadTrans outer, MonadConfig inner) => MonadConfig (outer inner) where
  getBaseUrl = lift getBaseUrl