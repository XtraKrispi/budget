module Effects.Scratch where

import Model
import Relude

class (Monad m) => MonadScratch m where
  get :: Email -> m (Maybe Scratch)
  save :: Email -> Scratch -> m ()

instance (MonadTrans outer, MonadScratch inner) => MonadScratch (outer inner) where
  get = lift . Effects.Scratch.get
  save email scratch = lift $ save email scratch