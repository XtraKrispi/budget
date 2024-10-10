{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.ArchiveStore where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Model (ArchivedItem, Email)

data ArchiveStore :: Effect where
  GetAll :: Email -> ArchiveStore m [ArchivedItem]
  Insert :: Email -> ArchivedItem -> ArchiveStore m ()

makeEffect ''ArchiveStore