{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.MakeMyUUID where

import Effectful
import Effectful.TH (makeEffect)
import MyUUID

data MakeMyUUID :: Effect where
  Generate :: MakeMyUUID m MyUUID

makeEffect ''MakeMyUUID
