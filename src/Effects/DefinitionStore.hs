{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.DefinitionStore where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Model.Definition
import Model.Email
import Model.Id

data DefinitionStore :: Effect where
  GetAll :: Email -> DefinitionStore m [Definition]
  Get :: Email -> Id Definition -> DefinitionStore m (Maybe Definition)
  Save :: Email -> Definition -> DefinitionStore m ()

makeEffect ''DefinitionStore