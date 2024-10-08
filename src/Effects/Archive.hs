{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Archive where

import Effectful
import Effectful.TH
import Model (ArchivedItem, Email)
import Relude

data Archive :: Effect where
  GetAll' :: Email -> Archive m [ArchivedItem]
  Insert' :: Email -> ArchivedItem -> Archive m ()

makeEffect ''Archive

class (Monad m) => MonadArchive m where
  insertArchive :: Email -> ArchivedItem -> m ()
  getAll :: Email -> m [ArchivedItem]

instance (MonadTrans outer, MonadArchive inner) => MonadArchive (outer inner) where
  insertArchive email item = lift $ insertArchive email item
  getAll = lift . Effects.Archive.getAll