module Web.View.Definitions.Show where
import Web.View.Prelude

data ShowView = ShowView { definition :: Definition }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={DefinitionsAction}>Definitions</a></li>
                <li class="breadcrumb-item active">Show Definition</li>
            </ol>
        </nav>
        <h1>Show Definition</h1>
        <p>{definition}</p>
    |]
