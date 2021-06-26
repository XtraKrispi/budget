module Web.View.Definitions.Edit where
import Web.View.Prelude

data EditView = EditView { definition :: Definition }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={DefinitionsAction}>Definitions</a></li>
                <li class="breadcrumb-item active">Edit Definition</li>
            </ol>
        </nav>
        <h1>Edit Definition</h1>
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
