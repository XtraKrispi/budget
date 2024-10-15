module Main (main) where

import App
import Configuration.Dotenv qualified as Dotenv
import System.Envy (decodeEnv)
import System.IO (hPutStrLn, stderr)
import Data.Text.IO qualified as TIO
import Db.Init qualified as InitDb
import Data.Text (pack)
import Web.Scotty
import Effectful
import Interpreters.WebServer 
import Effects.WebServer

main :: IO ()
main = do
  Dotenv.onMissingFile (Dotenv.loadFile Dotenv.defaultConfig) (pure ())
  eEnvironment <- decodeEnv
  case eEnvironment of
    Left err -> do
      hPutStrLn stderr "Cannot start application, environment variables not set!"
      hPutStrLn stderr err
    Right environment -> do
      results <- runEffects environment InitDb.initialize
      case results of
        Left (_callstack, err) -> TIO.putStrLn $ "There was a problem initializing the database: " <> pack (show err)
        Right _ -> pure ()
      app <- scottyApp (appMiddleware environment >> webapp environment)
      runEff $ runWebServerWarp (serve 8080 app)