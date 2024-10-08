{-# LANGUAGE RecordWildCards #-}

module Handlers.Definition where

import Effects.Definition (MonadDefinition)
import Effects.Definition qualified
import Effects.Definition qualified as Effect.Definition
import Effects.Time (MonadTime)
import Effects.Time qualified
import Handlers.Global (errorToast)
import Html.Definition (definitions, definitionsModal, definitionsPage)
import Htmx.Request (isBoosted, isHtmx)
import Id qualified
import Lucid
import Model
import Relude
import Web.Scotty.ActionT (captureParamMaybe, optionalFormParam, toggleFormParam)
import Web.Scotty.Trans (ActionT, formParam, html, setHeader)

getDefinitionPage ::
  ( MonadIO m
  , MonadDefinition m
  ) =>
  User ->
  ActionT m ()
getDefinitionPage user = do
  htmx <- isHtmx
  boosted <- isBoosted
  if htmx && not boosted
    then do
      ds <- Effects.Definition.getAll user.email
      html $ renderText $ definitions ds
    else do
      html $ renderText $ definitionsPage user

getDefinitionEdit ::
  ( MonadIO m
  , MonadTime m
  , MonadDefinition m
  ) =>
  User ->
  ActionT m ()
getDefinitionEdit _user = do
  mId <- captureParamMaybe "defId"
  today <- Effects.Time.today
  newId <- Id.newId
  mDefinition <- case mId of
    Just i -> Effects.Definition.getOne i
    Nothing ->
      pure $
        pure $
          Definition
            { definitionId = newId
            , definitionDescription = ""
            , definitionAmount = 0
            , definitionFrequency = OneTime
            , definitionStartDate = today
            , definitionEndDate = Nothing
            , definitionIsAutomaticWithdrawal = False
            }

  case mDefinition of
    (Just definition) -> do
      setHeader "HX-Trigger-After-Settle" "showDefinitionsModal"
      html $ renderText $ definitionsModal $ Just definition
    _ -> errorToast "There was an issue fetching the definition.  Please try again."

postDefinitionEdit ::
  ( MonadIO m
  , MonadDefinition m
  ) =>
  User ->
  ActionT m ()
postDefinitionEdit user = do
  definitionId <- formParam "id"
  definitionDescription <- formParam "description"
  definitionAmount <- formParam "amount"
  definitionFrequency <- formParam "frequency"
  definitionIsAutomaticWithdrawal <- toggleFormParam "is-automatic-withdrawal"
  definitionStartDate <- unMyDay <$> formParam "start-date"
  definitionEndDate <- fmap unMyDay <$> optionalFormParam "end-date"
  Effect.Definition.save user.email Definition{..}
  setHeader "HX-Trigger" "hideDefinitionsModal, reload"
