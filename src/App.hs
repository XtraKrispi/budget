module App where

-- newtype App es a = App {unApp :: (IOE :> es) => ReaderT Environment (ExceptT AppError (Eff es)) a}

-- instance MonadUnliftIO App where
--   withRunInIO inner =
--     App $ ReaderT $ \env ->
--       ExceptT $ try $ do
--         withRunInIO $ \runInIO ->
--           inner (runInIO . (either throwIO pure <=< runApp env))

-- runApp :: Environment -> App a -> IO (Either AppError a)
-- runApp env = runExceptT . flip runReaderT env . unApp

-- instance MonadSession App where
--   newSession :: Email -> ExpirationTime -> App SessionId
--   newSession email (ExpirationTime ex) = SessionDb.createSession email ex

--   slideSession :: SessionId -> ExpirationTime -> App ()
--   slideSession sessionId (ExpirationTime ex) = SessionDb.updateSession sessionId ex

--   getSessionUser :: SessionId -> App (Maybe (User, ExpirationTime))
--   getSessionUser sessionId = do
--     fmap (second ExpirationTime) <$> SessionDb.getUserForSession sessionId
--   logout :: SessionId -> App ()
--   logout = SessionDb.deleteSession

-- instance MonadTime App where
--   now = liftIO getCurrentTime

-- instance MonadUser App where
--   getUser = UserDb.getUserByEmail
--   insert = UserDb.insertUser
--   updatePassword = UserDb.updateUserPassword

-- instance MonadScratch App where
--   get = ScratchDb.getScratch
--   save = ScratchDb.saveUserScratch

-- instance MonadArchive App where
--   insertArchive = ArchiveDb.insertArchive
--   getAll = ArchiveDb.getAllArchive

-- instance MonadResetPassword App where
--   generateToken = ResetPassword.generateToken
--   insertToken email (ExpirationTime ex) token = do
--     results <- UserDb.insertResetToken email token ex
--     case results of
--       Nothing -> throwError NotFoundError
--       _ -> pure ()
--   getUsers = fmap (fmap (\(user, ex, token) -> (user, ExpirationTime ex, token))) UserDb.getUsersForResetPassword
--   removeUserTokens = UserDb.removeAllUserTokens

-- instance MonadDefinition App where
--   getAll = DefinitionDb.getAllDefinitions
--   getOne = DefinitionDb.getDefinitionById
--   save = DefinitionDb.upsertDefinition

-- instance MonadMyUUID App where
--   generate = MyUUID.nextRandom

-- instance MonadId App where
--   generate = Id.newId

-- instance MonadMail App where
--   sendMail to (Subject subject) body =
--     do
--       smtpConfig <- asks smtp
--       let mail =
--             simpleMail
--               ( Address
--                   (Just smtpConfig.smtpFromName)
--                   smtpConfig.smtpFromEmail
--               )
--               [Address Nothing (unEmail to)]
--               []
--               []
--               subject
--               [htmlPart (renderText body)]
--       liftIO $
--         sendMailWithLoginTLS smtpConfig.smtpHostname smtpConfig.smtpUsername smtpConfig.smtpPassword mail

-- instance MonadConfig App where
--   getBaseUrl = asks baseUrl

-- instance MonadPassword App where
--   hashPassword = Password.hashPassword