module Effects.Definition where

import Id
import Model
import Relude

class (Monad m) => MonadDefinition m where
  getAll :: Email -> m [Definition]
  getOne :: Id Definition -> m (Maybe Definition)
  save :: Email -> Definition -> m ()

instance (MonadTrans outer, MonadDefinition inner) => MonadDefinition (outer inner) where
  getAll = lift . Effects.Definition.getAll
  getOne = lift . getOne
  save email definition = lift $ save email definition