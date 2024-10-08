module Effects.Session where

import Model
import Relude

class (Monad m) => MonadSession m where
  newSession :: Email -> ExpirationTime -> m SessionId
  slideSession :: SessionId -> ExpirationTime -> m ()
  getSessionUser :: SessionId -> m (Maybe (User, ExpirationTime))
  logout :: SessionId -> m ()

instance (MonadTrans outer, MonadSession inner) => MonadSession (outer inner) where
  newSession email expiration = lift $ newSession email expiration
  slideSession sessionId expiration = lift $ slideSession sessionId expiration
  getSessionUser = lift . getSessionUser
  logout = lift . logout