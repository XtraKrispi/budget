module Interpreters.SessionStore where

import Data.Bifunctor (second)
import Db.Session qualified as SessionDb
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Static (Reader)
import Effects.SessionStore (SessionStore (..))
import Environment
import Model (ExpirationTime (..))

runSessionStoreSqlite :: (IOE :> es, Reader Environment :> es) => Eff (SessionStore : es) a -> Eff es a
runSessionStoreSqlite = interpret \_ -> \case
  NewSession email (ExpirationTime expirationTime) -> SessionDb.createSession email expirationTime
  SlideSession sessionId (ExpirationTime expirationTime) -> SessionDb.updateSession sessionId expirationTime
  GetSessionUser sessionId -> fmap (second ExpirationTime) <$> SessionDb.getUserForSession sessionId
  Logout sessionId -> SessionDb.deleteSession sessionId