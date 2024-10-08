module Effects.Id where

import Id
import Relude

class (Monad m) => MonadId m where
  generate :: m (Id a)

instance (MonadTrans outer, MonadId inner) => MonadId (outer inner) where
  generate = lift generate