module Id where

import Effectful
import Effects.MakeMyUUID (MakeMyUUID, generate)
import Model.Id

newId :: (MakeMyUUID :> es) => Eff es (Id a)
newId = Id <$> generate