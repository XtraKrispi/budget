{-# LANGUAGE RecordWildCards #-}

module Handlers.Definition where

import Effects.Definition (MonadDefinition)
import Effects.Definition qualified
import Effects.Definition qualified as Effect.Definition
import Effects.Id
import Effects.Time (MonadTime)
import Effects.Time qualified
import Effects.WebServer (MonadWebServer (..))
import Handlers.Global (errorToast)
import Html.Definition (definitions, definitionsModal, definitionsPage)
import Htmx.Request (isBoosted, isHtmx)
import Model
import Relude

getDefinitionPage ::
  ( MonadDefinition m
  , MonadWebServer m
  ) =>
  User ->
  m ()
getDefinitionPage user = do
  htmx <- isHtmx
  boosted <- isBoosted
  if htmx && not boosted
    then do
      ds <- Effects.Definition.getAll user.email
      serveHtml $ definitions ds
    else do
      serveHtml $ definitionsPage user

getDefinitionEdit ::
  ( MonadWebServer m
  , MonadTime m
  , MonadDefinition m
  , MonadId m
  ) =>
  User ->
  m ()
getDefinitionEdit _user = do
  mId <- fromUrl "defId"
  today <- Effects.Time.today
  newId <- Effects.Id.generate
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
      setResponseHeader "HX-Trigger-After-Settle" "showDefinitionsModal"
      serveHtml $ definitionsModal $ Just definition
    _ -> errorToast "There was an issue fetching the definition.  Please try again."

postDefinitionEdit ::
  ( MonadWebServer m
  , MonadDefinition m
  ) =>
  User ->
  m ()
postDefinitionEdit user = do
  definitionId <- fromForm "id"
  definitionDescription <- fromForm "description"
  definitionAmount <- fromForm "amount"
  definitionFrequency <- fromForm "frequency"
  definitionIsAutomaticWithdrawal <- fmap (== ("on" :: Text)) <$> fromForm "is-automatic-withdrawal"
  definitionStartDate <- fmap unMyDay <$> fromForm "start-date"
  definitionEndDate <- fmap unMyDay <$> fromForm "end-date"
  let mDefinition =
        Definition
          <$> definitionId
          <*> definitionDescription
          <*> definitionAmount
          <*> definitionFrequency
          <*> definitionStartDate
          <*> pure definitionEndDate
          <*> definitionIsAutomaticWithdrawal

  case mDefinition of
    Just def -> do
      Effect.Definition.save user.email def
      setResponseHeader "HX-Trigger" "hideDefinitionsModal, reload"
    Nothing -> errorToast "There was an issue saving the definition. Please try again."
