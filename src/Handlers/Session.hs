module Handlers.Session where

import Auth qualified
import Control.Monad.Trans (lift)
import Effectful
import Effectful.Reader.Static (Reader)
import Effects.SessionStore
import Environment (Environment)
import Model (User)
import Web.Scotty.Trans (ActionT, setHeader)

deleteSession ::
  ( SessionStore :> es
  , Reader Environment :> es
  , IOE :> es
  ) =>
  User ->
  ActionT (Eff es) ()
deleteSession _ = do
  mSessionId <- Auth.getAuthCookie
  case mSessionId of
    Just sessionId -> do
      lift $ Effects.SessionStore.logout sessionId
      Auth.invalidateAuthCookie
    Nothing -> pure ()

  setHeader "HX-Redirect" "/"
