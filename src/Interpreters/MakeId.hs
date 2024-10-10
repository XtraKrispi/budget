module Interpreters.MakeId where

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effects.MakeId (MakeId (..))
import Effects.MakeMyUUID (MakeMyUUID)
import Id qualified

runMakeIdIO :: (MakeMyUUID :> es) => Eff (MakeId : es) a -> Eff es a
runMakeIdIO = interpret \_ -> \case
  Generate -> Id.newId