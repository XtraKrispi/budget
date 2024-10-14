{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.UserStore where

import Effectful
import Effectful.TH (makeEffect)
import Model.Common
import Model.Email
import Model.Password
import Model.User

data UserStore :: Effect where
  Get :: Email -> UserStore m (Maybe User)
  Insert :: User -> UserStore m ()
  UpdatePassword :: Email -> Password Hashed -> UserStore m ()

makeEffect ''UserStore