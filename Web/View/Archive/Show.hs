module Web.View.Archive.Show where
import Web.View.Prelude

data ShowView = ShowView { archive :: Archive }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ArchivesAction}>Archives</a></li>
                <li class="breadcrumb-item active">Show Archive</li>
            </ol>
        </nav>
        <h1>Show Archive</h1>
        <p>{archive}</p>
    |]
