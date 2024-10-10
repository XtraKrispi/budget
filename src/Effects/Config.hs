module Effects.Config where

import Control.Monad.Trans (MonadTrans, lift)
import Environment (BaseUrl)

class (Monad m) => MonadConfig m where
  getBaseUrl :: m BaseUrl

instance (MonadTrans outer, MonadConfig inner) => MonadConfig (outer inner) where
  getBaseUrl = lift getBaseUrl