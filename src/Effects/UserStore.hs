{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.UserStore where

import Effectful
import Effectful.TH (makeEffect)
import Model

data UserStore :: Effect where
  Get :: Email -> UserStore m (Maybe User)
  Insert :: User -> UserStore m ()
  UpdatePassword :: Email -> Password Hashed -> UserStore m ()

makeEffect ''UserStore