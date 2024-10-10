module Handlers.Definition where

import Control.Monad.Trans (lift)
import Effectful
import Effects.DefinitionStore
import Effects.DefinitionStore qualified as Effect.DefinitionStore
import Effects.MakeId
import Effects.Time (Time)
import Effects.Time qualified
import Handlers.Global (errorToast)
import Html.Definition (definitions, definitionsModal, definitionsPage)
import Htmx.Request (isBoosted, isHtmx)
import Model
import Web.Scotty.ActionT (captureParamMaybe, optionalFormParam, renderHtml, toggleFormParam)
import Web.Scotty.Trans (ActionT, formParam, setHeader)

getDefinitionPage ::
  (DefinitionStore :> es, IOE :> es) =>
  User ->
  ActionT (Eff es) ()
getDefinitionPage user = do
  htmx <- isHtmx
  boosted <- isBoosted
  if htmx && not boosted
    then do
      ds <- lift $ Effects.DefinitionStore.getAll user.email
      renderHtml $ definitions ds
    else do
      renderHtml $ definitionsPage user

getDefinitionEdit ::
  ( Time :> es
  , DefinitionStore :> es
  , MakeId :> es
  , IOE :> es
  ) =>
  User ->
  ActionT (Eff es) ()
getDefinitionEdit user = do
  mId <- captureParamMaybe "defId"
  today <- lift Effects.Time.today
  newId <- lift Effects.MakeId.generate
  mDefinition <- case mId of
    Just i -> lift $ Effects.DefinitionStore.get user.email i
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
      renderHtml $ definitionsModal $ Just definition
    _ -> errorToast "There was an issue fetching the definition.  Please try again."

postDefinitionEdit ::
  ( DefinitionStore :> es
  , IOE :> es
  ) =>
  User ->
  ActionT (Eff es) ()
postDefinitionEdit user = do
  definitionId <- formParam "id"
  definitionDescription <- formParam "description"
  definitionAmount <- formParam "amount"
  definitionFrequency <- formParam "frequency"
  definitionIsAutomaticWithdrawal <- toggleFormParam "is-automatic-withdrawal"
  definitionStartDate <- unMyDay <$> formParam "start-date"
  definitionEndDate <- fmap unMyDay <$> optionalFormParam "end-date"
  let definition =
        Definition
          definitionId
          definitionDescription
          definitionAmount
          definitionFrequency
          definitionStartDate
          definitionEndDate
          definitionIsAutomaticWithdrawal

  lift $ Effect.DefinitionStore.save user.email definition
  setHeader "HX-Trigger" "hideDefinitionsModal, reload"
