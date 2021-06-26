module Web.View.Archive.New where
import Web.View.Prelude

data NewView = NewView { archive :: Archive }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ArchivesAction}>Archives</a></li>
                <li class="breadcrumb-item active">New Archive</li>
            </ol>
        </nav>
        <h1>New Archive</h1>
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
