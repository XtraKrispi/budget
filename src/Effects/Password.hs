module Effects.Password where

import Model
import Relude

class (Monad m) => MonadPassword m where
  hashPassword :: Password PlainText -> m (Password Hashed)

instance (MonadTrans outer, MonadPassword inner) => MonadPassword (outer inner) where
  hashPassword = lift . hashPassword