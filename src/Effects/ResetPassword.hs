module Effects.ResetPassword where

import Model
import Relude

class (Monad m) => MonadResetPassword m where
  generateToken :: m (Token PlainText, Token Hashed)
  insertToken :: Email -> ExpirationTime -> Token Hashed -> m ()
  getUsers :: m [(User, ExpirationTime, Token Hashed)]
  removeUserTokens :: Email -> m ()

instance (MonadTrans outer, MonadResetPassword inner) => MonadResetPassword (outer inner) where
  generateToken = lift generateToken
  insertToken email expiry token = lift $ insertToken email expiry token
  getUsers = lift getUsers
  removeUserTokens = lift . removeUserTokens