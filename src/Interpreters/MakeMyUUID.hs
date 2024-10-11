module Interpreters.MakeMyUUID where

import Data.UUID qualified as UUID
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effects.MakeMyUUID (MakeMyUUID (..))
import MyUUID (MyUUID (..), nextRandom)

runMakeMyUUIDIO :: (IOE :> es) => Eff (MakeMyUUID : es) a -> Eff es a
runMakeMyUUIDIO = interpret \_ -> \case
  Generate -> liftIO MyUUID.nextRandom

runMakeMyUUIDPure :: Eff (MakeMyUUID : es) a -> Eff es a
runMakeMyUUIDPure = interpret \_ -> \case
  Generate -> pure $ MyUUID UUID.nil