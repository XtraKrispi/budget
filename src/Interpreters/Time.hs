module Interpreters.Time where

import Data.Time (UTCTime (utctDay), getCurrentTime)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effects.Time

runTimeIO :: (IOE :> es) => Eff (Time : es) a -> Eff es a
runTimeIO = interpret \_ -> \case
  Now -> liftIO getCurrentTime
  Today -> liftIO $ utctDay <$> getCurrentTime