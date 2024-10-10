{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Auth where

-- data Auth :: Effect where
--   GetAuthCookie :: Auth m (Maybe SessionId)
--   SetAuthCookie :: SessionId -> Auth m ()
--   InvalidateAuthCookie :: Auth m ()

-- makeEffect ''Auth

-- class (Monad m) => MonadAuth m where
--   getAuthCookie :: m (Maybe SessionId)
--   setAuthCookie :: SessionId -> m ()
--   invalidateAuthCookie :: m ()

-- instance (MonadIO m, MonadReader env m, HasAuthCookieName env) => MonadAuth (ActionT m) where
--   getAuthCookie = do
--     cookieName <- lift $ asks authCookieName
--     mCookie <- getCookie cookieName
--     case mCookie of
--       Just cookie ->
--         pure $ SessionId <$> MyUUID.fromText cookie
--       Nothing -> pure Nothing
--   setAuthCookie sessionId = do
--     cookieName <- lift $ asks authCookieName
--     setCookie
--       defaultSetCookie
--         { setCookieName = encodeUtf8 cookieName
--         , setCookieValue = encodeUtf8 $ MyUUID.toText $ unSessionId sessionId
--         , setCookieMaxAge = Just 1200
--         , setCookieHttpOnly = True
--         , setCookieSecure = True
--         }

--   invalidateAuthCookie = do
--     cookieName <- lift $ asks authCookieName
--     setCookie
--       defaultSetCookie
--         { setCookieName = encodeUtf8 cookieName
--         , setCookieValue = ""
--         , setCookieMaxAge = Just (-100)
--         , setCookieHttpOnly = True
--         , setCookieSecure = True
--         }