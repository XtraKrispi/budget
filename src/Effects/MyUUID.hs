module Effects.MyUUID where

import MyUUID
import Relude

class (Monad m) => MonadMyUUID m where
  generate :: m MyUUID

instance (MonadTrans outer, MonadMyUUID inner) => MonadMyUUID (outer inner) where
  generate = lift generate