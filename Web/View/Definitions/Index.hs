module Web.View.Definitions.Index where

import Web.View.Prelude

newtype IndexView = IndexView {definitions :: [Definition]}

instance View IndexView where
  html IndexView {..} =
    [hsx|
        <h1>Definitions <a href={pathTo NewDefinitionAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div>
            {forEach definitions renderDefinition}
        </div>
    |]

renderDefinition :: Definition -> Html
renderDefinition definition =
  [hsx|
  <div class="card mb-2">
    <div class="card-header d-flex flex-row justify-content-between">
        <span><strong>{get #description definition}</strong></span>
        <span class={classes [("text-danger", amountType definition == Debit)
                            , ("text-success", amountType definition == Credit)]}>
            <strong>{get #amount definition |> formatCurrency}</strong>
        </span>
    </div>
    <div class="card-body">
        <div>Frequency: {get #frequency definition |> prettyFreq}</div>
        <div>Starting: {get #startDate definition |> isoDate}</div>
    </div>
     <div class="card-footer">
        <div class="btn-group">
            <a class="btn btn-primary js-delete" href={DeleteDefinitionAction (get #id definition)}>Delete</a>
            <a class="btn btn-primary" onclick="return confirm('Are you sure you want to end this item?')" href={DeleteDefinitionAction (get #id definition)}>End</a>
        </div>
     </div>
  </div>
|]