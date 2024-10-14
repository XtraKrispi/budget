{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.ResetPasswordStore where

import Effectful
import Effectful.TH (makeEffect)
import Model.Common
import Model.Email
import Model.Token
import Model.User

data ResetPasswordStore :: Effect where
  GenerateToken :: ResetPasswordStore m (Token PlainText, Token Hashed)
  InsertToken :: Email -> ExpirationTime -> Token Hashed -> ResetPasswordStore m ()
  GetUsers :: ResetPasswordStore m [(User, ExpirationTime, Token Hashed)]
  RemoveUserTokens :: Email -> ResetPasswordStore m ()

makeEffect ''ResetPasswordStore