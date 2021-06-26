module Web.View.Definitions.New where
import Web.View.Prelude

data NewView = NewView { definition :: Definition }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={DefinitionsAction}>Definitions</a></li>
                <li class="breadcrumb-item active">New Definition</li>
            </ol>
        </nav>
        <h1>New Definition</h1>
        {renderForm definition}
    |]

renderForm :: Definition -> Html
renderForm definition = formFor definition [hsx|
    {(textField #description)}
    {(textField #amount)}
    {(textField #amountType)}
    {(textField #startDate)}
    {(textField #endDate)}
    {(textField #frequency)}
    {submitButton}
|]
