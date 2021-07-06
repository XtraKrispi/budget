module Web.View.Home.Index where

import Web.View.Prelude

newtype IndexView = IndexView { items :: [BudgetItem] }

instance View IndexView where
  html IndexView { items } =
    [hsx|
        <div>Home Page</div>
        {forEach items renderItem}
    |]

renderItem :: BudgetItem -> Html
renderItem i = [hsx|
  <div class="card">
    {get #description i}
    {get #date i}
  </div>
|]