module Handlers.Definition where

import AppError (AppError)
import Effectful
import Effectful.Error.Static
import Effects.DefinitionStore
import Effects.DefinitionStore qualified as Effect.DefinitionStore
import Effects.MakeId
import Effects.Time (Time)
import Effects.Time qualified
import Handlers.Model (Request, Response)
import Handlers.Utils
import Html.Definition (definitions, definitionsModal, definitionsPage)
import Htmx.Request (isBoosted, isHtmx)
import Model

getDefinitionPage ::
  (DefinitionStore :> es) =>
  Request ->
  User ->
  Eff es Response
getDefinitionPage request user = do
  let htmx = isHtmx request
  let boosted = isBoosted request
  if htmx && not boosted
    then do
      ds <- Effects.DefinitionStore.getAll user.email
      pure $ htmlResponse $ definitions ds
    else do
      pure $ htmlResponse $ definitionsPage user

getDefinitionEdit ::
  ( Time :> es
  , DefinitionStore :> es
  , MakeId :> es
  , Error AppError :> es
  ) =>
  Request ->
  User ->
  Eff es Response
getDefinitionEdit request user = do
  mId <- getParamMaybe request "defId"
  today <- Effects.Time.today
  newId <- Effects.MakeId.generate
  mDefinition <- case mId of
    Just i -> Effects.DefinitionStore.get user.email i
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
      pure $ makeResponse [("HX-Trigger-After-Settle", "showDefinitionsModal")] [] (definitionsModal $ Just definition)
    _ -> pure $ errorResponse "There was an issue fetching the definition.  Please try again."

postDefinitionEdit ::
  ( DefinitionStore :> es
  , Error AppError :> es
  ) =>
  Request ->
  User ->
  Eff es Response
postDefinitionEdit request user = do
  definitionId <- getParam request "id"
  definitionDescription <- getParam request "description"
  definitionAmount <- getParam request "amount"
  definitionFrequency <- getParam request "frequency"
  let definitionIsAutomaticWithdrawal = getToggleParam request "is-automatic-withdrawal"
  definitionStartDate <- unMyDay <$> getParam request "start-date"
  definitionEndDate <- fmap unMyDay <$> getParamMaybe request "end-date"
  let definition =
        Definition
          definitionId
          definitionDescription
          definitionAmount
          definitionFrequency
          definitionStartDate
          definitionEndDate
          definitionIsAutomaticWithdrawal

  Effect.DefinitionStore.save user.email definition
  pure $ headerResponse "HX-Trigger" "hideDefinitionsModal, reload"
