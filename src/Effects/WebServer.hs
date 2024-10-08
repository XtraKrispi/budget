module Effects.WebServer where

import Debug.Trace qualified as Debug
import Lucid (Html, renderText)
import Relude
import Web.Scotty.Trans

class (Monad m) => MonadWebServer m where
  fromForm :: (Read a) => Text -> m (Maybe a)
  fromUrl :: (Read a) => Text -> m (Maybe a)
  setResponseHeader :: Text -> Text -> m ()
  getRequestHeader :: Text -> m (Maybe Text)
  redirect :: Text -> m ()
  getCookie :: Text -> m (Maybe Text)
  serveHtml :: Html () -> m ()

instance (MonadIO m) => MonadWebServer (ActionT m) where
  fromForm p = do
    p' <- formParam (fromStrict p)
    pure $ readMaybe (Debug.trace (show p') p')
  fromUrl = fmap readMaybe <$> captureParam . fromStrict

  setResponseHeader = setHeader `on` fromStrict
  getRequestHeader h = fmap toStrict <$> header (fromStrict h)
  redirect = Web.Scotty.Trans.redirect . fromStrict
  getCookie = getCookie
  serveHtml = html . renderText