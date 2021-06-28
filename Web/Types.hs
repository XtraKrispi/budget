module Web.Types where

import Generated.Types
import IHP.LoginSupport.Types
import IHP.ModelSupport
import IHP.Prelude

data WebApplication = WebApplication deriving (Eq, Show)

data StaticController = WelcomeAction deriving (Eq, Show, Data)

data HomeController
  = HomeAction
  deriving (Eq, Show, Data)

data DefinitionsController
  = DefinitionsAction
  | NewDefinitionAction
  | ShowDefinitionAction {definitionId :: !(Id Definition)}
  | CreateDefinitionAction
  | DeleteDefinitionAction {definitionId :: !(Id Definition)}
  deriving (Eq, Show, Data)

data ArchiveController
  = ArchivesAction
  | NewArchiveAction
  | ShowArchiveAction {archiveId :: !(Id Archive)}
  | CreateArchiveAction
  | EditArchiveAction {archiveId :: !(Id Archive)}
  | UpdateArchiveAction {archiveId :: !(Id Archive)}
  | DeleteArchiveAction {archiveId :: !(Id Archive)}
  deriving (Eq, Show, Data)

instance HasNewSessionUrl User where
  newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User

data SessionsController
  = NewSessionAction
  | CreateSessionAction
  | DeleteSessionAction
  deriving (Eq, Show, Data)