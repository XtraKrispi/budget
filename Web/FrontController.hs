module Web.FrontController where

-- Controller Imports

import IHP.LoginSupport.Middleware
import IHP.RouterPrelude
import Web.Controller.Archive
import Web.Controller.Definitions
import Web.Controller.Home
import Web.Controller.Prelude
import Web.Controller.Sessions
import Web.Controller.Static
import Web.View.Layout (defaultLayout)

instance FrontController WebApplication where
  controllers =
    [ startPage HomeAction,
      -- Generator Marker
      parseRoute @SessionsController,
      parseRoute @ArchiveController,
      parseRoute @DefinitionsController
    ]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout
    initAutoRefresh
    initAuthentication @User
