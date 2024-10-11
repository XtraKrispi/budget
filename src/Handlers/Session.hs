module Handlers.Session where

import Auth qualified
import Effectful
import Effectful.Reader.Static (Reader)
import Effects.SessionStore
import Environment (Environment)
import Handlers.Model
import Handlers.Utils (makeResponse)

deleteSession ::
  ( SessionStore :> es
  , Reader Environment :> es
  ) =>
  Request ->
  Eff es Response
deleteSession request = do
  mSessionId <- Auth.getAuthCookie request
  case mSessionId of
    Just sessionId -> do
      Effects.SessionStore.logout sessionId
      authCookie <- Auth.invalidateAuthCookie
      pure $ makeResponse [("HX-Redirect", "/")] [authCookie] mempty
    Nothing -> pure $ makeResponse [("HX-Redirect", "/")] [] mempty
