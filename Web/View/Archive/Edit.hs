module Web.View.Archive.Edit where
import Web.View.Prelude

data EditView = EditView { archive :: Archive }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ArchivesAction}>Archives</a></li>
                <li class="breadcrumb-item active">Edit Archive</li>
            </ol>
        </nav>
        <h1>Edit Archive</h1>
        {renderForm archive}
    |]

renderForm :: Archive -> Html
renderForm archive = formFor archive [hsx|
    {(textField #definitionId)}
    {(textField #description)}
    {(textField #amount)}
    {(textField #date)}
    {submitButton}
|]
