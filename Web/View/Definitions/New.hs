module Web.View.Definitions.New where

import Web.View.Prelude

data NewView = NewView {definition :: Definition, defaultStartDate :: Day}

instance View NewView where
  html NewView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={DefinitionsAction}>Definitions</a></li>
                <li class="breadcrumb-item active">New Definition</li>
            </ol>
        </nav>
        <h1>New Definition</h1>
        {renderForm defaultStartDate definition}
    |]

renderForm :: Day -> Definition -> Html
renderForm defaultStartDate definition =
  formFor
    definition
    [hsx|
    {(textField #description)}
    {(selectField #frequency [Onetime, Weekly, Biweekly, Monthly])}
    {(selectField #amountType [Debit, Credit])}
    <div class="form-group">
    <label for="definition_amount">Amount</label>
    <div class="input-group">
        <div class="input-group-prepend">
            <span class="input-group-text">$</span>
        </div>
        {(textField #amount) { disableLabel = True, disableGroup = True}}
    </div>
    </div>
    {(dateField #startDate) {fieldValue = isoDate defaultStartDate }}
    {(dateField #endDate)}
    {submitButton}
|]
