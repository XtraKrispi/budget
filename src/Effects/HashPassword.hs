{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.HashPassword where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Model (Hashed, Password, PlainText)

data HashPassword :: Effect where
  HashPassword :: Password PlainText -> HashPassword m (Password Hashed)

makeEffect ''HashPassword