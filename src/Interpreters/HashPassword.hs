module Interpreters.HashPassword where

import Effectful (Eff, IOE, MonadIO (liftIO), type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effects.HashPassword (HashPassword (..))
import Model (Password (..))
import Password qualified

runHashPasswordIO :: (IOE :> es) => Eff (HashPassword : es) a -> Eff es a
runHashPasswordIO = interpret \_ -> \case
  HashPassword pass -> liftIO $ Password.hashPassword pass

runHashPasswordPure :: Eff (HashPassword : es) a -> Eff es a
runHashPasswordPure = interpret \_ -> \case
  HashPassword (Password pass) -> pure $ Password ("HASHED:" <> pass)