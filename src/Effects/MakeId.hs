{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.MakeId where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Model.Id

data MakeId :: Effect where
  Generate :: MakeId m (Id a)

makeEffect ''MakeId
