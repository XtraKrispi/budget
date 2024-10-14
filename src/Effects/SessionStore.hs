{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.SessionStore where

import Effectful
import Effectful.TH (makeEffect)
import Model.Common
import Model.Email
import Model.User

data SessionStore :: Effect where
  NewSession :: Email -> ExpirationTime -> SessionStore m SessionId
  SlideSession :: SessionId -> ExpirationTime -> SessionStore m ()
  GetSessionUser :: SessionId -> SessionStore m (Maybe (User, ExpirationTime))
  Logout :: SessionId -> SessionStore m ()

makeEffect ''SessionStore