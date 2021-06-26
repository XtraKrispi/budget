module Web.Controller.Definitions where

import Web.Controller.Prelude
import Web.View.Definitions.Index
import Web.View.Definitions.New
import Web.View.Definitions.Edit
import Web.View.Definitions.Show

instance Controller DefinitionsController where
    action DefinitionsAction = do
        definitions <- query @Definition |> fetch
        render IndexView { .. }

    action NewDefinitionAction = do
        let definition = newRecord
        render NewView { .. }

    action ShowDefinitionAction { definitionId } = do
        definition <- fetch definitionId
        render ShowView { .. }

    action EditDefinitionAction { definitionId } = do
        definition <- fetch definitionId
        render EditView { .. }

    action UpdateDefinitionAction { definitionId } = do
        definition <- fetch definitionId
        definition
            |> buildDefinition
            |> ifValid \case
                Left definition -> render EditView { .. }
                Right definition -> do
                    definition <- definition |> updateRecord
                    setSuccessMessage "Definition updated"
                    redirectTo EditDefinitionAction { .. }

    action CreateDefinitionAction = do
        let definition = newRecord @Definition
        definition
            |> buildDefinition
            |> ifValid \case
                Left definition -> render NewView { .. } 
                Right definition -> do
                    definition <- definition |> createRecord
                    setSuccessMessage "Definition created"
                    redirectTo DefinitionsAction

    action DeleteDefinitionAction { definitionId } = do
        definition <- fetch definitionId
        deleteRecord definition
        setSuccessMessage "Definition deleted"
        redirectTo DefinitionsAction

buildDefinition definition = definition
    |> fill @["description","amount","amountType","startDate","endDate","frequency"]
