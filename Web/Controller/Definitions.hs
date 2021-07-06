module Web.Controller.Definitions where

import Web.Controller.Prelude
import Web.View.Definitions.Index
import Web.View.Definitions.New
import Web.View.Definitions.Show

getDefaultStartDate :: IO Day
getDefaultStartDate = utctDay <$> getCurrentTime

instance Controller DefinitionsController where
  beforeAction = ensureIsUser
  action DefinitionsAction = do
    definitions <- query @Definition |> fetch
    render IndexView {..}
  action NewDefinitionAction = do
    let definition = newRecord
    defaultStartDate <- getDefaultStartDate
    render NewView {..}
  action ShowDefinitionAction {definitionId} = do
    definition <- fetch definitionId
    render ShowView {..}
  action CreateDefinitionAction = do
    let definition = newRecord @Definition
    definition
      |> buildDefinition
      |> validateField #description nonEmpty
      |> validateField #amount (isGreaterThan 0)
      |> ifValid \case
        Left definition -> do
          let defaultStartDate = startDate definition
          render NewView {..}
        Right definition -> do
          definition <- definition |> createRecord
          setSuccessMessage "Definition created"
          redirectTo DefinitionsAction
  action DeleteDefinitionAction {definitionId} = do
    definition <- fetch definitionId
    definition { isDeleted = True } |> updateRecord
    setSuccessMessage "Definition deleted"
    redirectTo DefinitionsAction

buildDefinition definition =
  definition
    |> fill @["description", "amount", "amountType", "startDate", "endDate", "frequency"]
