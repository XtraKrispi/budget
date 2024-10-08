module Effects.Archive where

import Model (ArchivedItem, Email)
import Relude

class (Monad m) => MonadArchive m where
  insertArchive :: Email -> ArchivedItem -> m ()
  getAll :: Email -> m [ArchivedItem]

instance (MonadTrans outer, MonadArchive inner) => MonadArchive (outer inner) where
  insertArchive email item = lift $ insertArchive email item
  getAll = lift . Effects.Archive.getAll