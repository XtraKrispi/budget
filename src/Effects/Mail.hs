module Effects.Mail where

import Lucid
import Model (Email)
import Relude

newtype Subject = Subject {unSubject :: Text}

class (Monad m) => MonadMail m where
  sendMail :: Email -> Subject -> Html () -> m ()

instance (MonadTrans outer, MonadMail inner) => MonadMail (outer inner) where
  sendMail to subject body = lift $ sendMail to subject body