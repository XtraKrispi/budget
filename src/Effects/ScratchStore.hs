{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.ScratchStore where

import Effectful
import Effectful.TH (makeEffect)
import Model.Email
import Model.Scratch

data ScratchStore :: Effect where
  Get :: Email -> ScratchStore m (Maybe Scratch)
  Save :: Email -> Scratch -> ScratchStore m ()

makeEffect ''ScratchStore