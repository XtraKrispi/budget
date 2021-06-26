module Web.View.Definitions.Index where
import Web.View.Prelude

data IndexView = IndexView { definitions :: [Definition] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={DefinitionsAction}>Definitions</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewDefinitionAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Definition</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach definitions renderDefinition}</tbody>
            </table>
        </div>
    |]


renderDefinition :: Definition -> Html
renderDefinition definition = [hsx|
    <tr>
        <td>{definition}</td>
        <td><a href={ShowDefinitionAction (get #id definition)}>Show</a></td>
        <td><a href={EditDefinitionAction (get #id definition)} class="text-muted">Edit</a></td>
        <td><a href={DeleteDefinitionAction (get #id definition)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
