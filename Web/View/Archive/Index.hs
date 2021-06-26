module Web.View.Archive.Index where
import Web.View.Prelude

data IndexView = IndexView { archive :: [Archive] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={ArchivesAction}>Archives</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewArchiveAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Archive</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach archive renderArchive}</tbody>
            </table>
        </div>
    |]


renderArchive :: Archive -> Html
renderArchive archive = [hsx|
    <tr>
        <td>{archive}</td>
        <td><a href={ShowArchiveAction (get #id archive)}>Show</a></td>
        <td><a href={EditArchiveAction (get #id archive)} class="text-muted">Edit</a></td>
        <td><a href={DeleteArchiveAction (get #id archive)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
