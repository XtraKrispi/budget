{-# LANGUAGE TemplateHaskell #-}

module Effects.WebServer where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Network.Wai (Application)

data WebServer :: Effect where
  Serve :: Int -> Application -> WebServer m ()

makeEffect ''WebServer
