{-# LANGUAGE RecordWildCards #-}

module Handlers.Definition where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, MonadTrans (lift))
import Data.Time (UTCTime (utctDay), getCurrentTime)
import Db qualified
import Environment
import Handlers.Global (errorToast)
import Html.Definition (definitions, definitionsModal, definitionsPage)
import Htmx.Request (isBoosted, isHtmx)
import Id qualified
import Lucid
import Model
import Web.Scotty.ActionT (captureParamMaybe, optionalFormParam, toggleFormParam)
import Web.Scotty.Auth (requiresAuth)
import Web.Scotty.Trans (ActionT, formParam, html, setHeader)

getDefinitionPage ::
  ( HasAuthCookieName env
  , HasAppEnvironment env
  , HasDbPath env
  , MonadIO m
  , MonadReader env m
  ) =>
  ActionT m ()
getDefinitionPage = requiresAuth \user -> do
  htmx <- isHtmx
  boosted <- isBoosted
  if htmx && not boosted
    then do
      defs <- lift $ Db.getAllDefinitions user.email
      case defs of
        Right ds -> html $ renderText $ definitions ds
        Left _ -> errorToast "There was an issue fetching definitions, please refresh the page and try again."
    else do
      html $ renderText $ definitionsPage user

getDefinitionEdit ::
  ( HasAuthCookieName env
  , HasAppEnvironment env
  , HasDbPath env
  , MonadIO m
  , MonadReader env m
  ) =>
  ActionT m ()
getDefinitionEdit = requiresAuth \_user -> do
  mId <- captureParamMaybe "defId"
  today <- liftIO $ fmap utctDay getCurrentTime
  newId <- Id.newId
  eDefinition <-
    lift $
      maybe
        ( pure
            ( Right
                ( Just
                    ( Definition
                        { definitionId = newId
                        , definitionDescription = ""
                        , definitionAmount = 0
                        , definitionFrequency = OneTime
                        , definitionStartDate = today
                        , definitionEndDate = Nothing
                        , definitionIsAutomaticWithdrawal = False
                        }
                    )
                )
            )
        )
        Db.getDefinitionById
        mId
  case eDefinition of
    Right (Just definition) -> do
      setHeader "HX-Trigger-After-Settle" "showDefinitionsModal"
      html $ renderText $ definitionsModal $ Just definition
    _ -> errorToast "There was an issue fetching the definition.  Please try again."

postDefinitionEdit ::
  ( HasAuthCookieName env
  , HasAppEnvironment env
  , HasDbPath env
  , MonadIO m
  , MonadReader env m
  ) =>
  ActionT m ()
postDefinitionEdit = requiresAuth \user -> do
  definitionId <- formParam "id"
  definitionDescription <- formParam "description"
  definitionAmount <- formParam "amount"
  definitionFrequency <- formParam "frequency"
  definitionIsAutomaticWithdrawal <- toggleFormParam "is-automatic-withdrawal"
  definitionStartDate <- unMyDay <$> formParam "start-date"
  definitionEndDate <- fmap unMyDay <$> optionalFormParam "end-date"

  results <- lift $ Db.upsertDefinition Definition{..} user.email
  case results of
    Right _ -> setHeader "HX-Trigger" "hideDefinitionsModal, reload"
    Left _ -> errorToast "Something went wrong, please try again."
