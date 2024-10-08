module Effects.User where

import Model
import Relude

class (Monad m) => MonadUser m where
  getUser :: Email -> m (Maybe User)
  insert :: User -> m ()
  updatePassword :: Email -> Password Hashed -> m ()

instance (MonadTrans outer, MonadUser inner) => MonadUser (outer inner) where
  getUser = lift . getUser
  insert = lift . insert
  updatePassword email p = lift $ updatePassword email p