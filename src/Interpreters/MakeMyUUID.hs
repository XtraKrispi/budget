module Interpreters.MakeMyUUID where

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effects.MakeMyUUID (MakeMyUUID (..))
import MyUUID (nextRandom)

runMakeMyUUIDIO :: (IOE :> es) => Eff (MakeMyUUID : es) a -> Eff es a
runMakeMyUUIDIO = interpret \_ -> \case
  Generate -> liftIO MyUUID.nextRandom