module Effects.Time where

import Data.Time (Day, UTCTime (utctDay))
import Relude

class (Monad m) => MonadTime m where
  now :: m UTCTime
  today :: m Day
  today = utctDay <$> now

instance (MonadTrans outer, MonadTime inner) => MonadTime (outer inner) where
  now = lift now
  today = lift today