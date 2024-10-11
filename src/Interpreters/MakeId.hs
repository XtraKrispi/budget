module Interpreters.MakeId where

import Data.UUID qualified as UUID
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effects.MakeId (MakeId (..))
import Effects.MakeMyUUID (MakeMyUUID)
import Id (Id (..))
import Id qualified
import MyUUID (MyUUID (MyUUID))

runMakeIdIO :: (MakeMyUUID :> es) => Eff (MakeId : es) a -> Eff es a
runMakeIdIO = interpret \_ -> \case
  Generate -> Id.newId

runMakeIdPure :: Eff (MakeId : es) a -> Eff es a
runMakeIdPure = interpret \_ -> \case
  Generate -> pure $ Id $ MyUUID UUID.nil