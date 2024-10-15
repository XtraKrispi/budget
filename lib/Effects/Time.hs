{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Time where

import Data.Time (Day, UTCTime)
import Effectful
import Effectful.TH (makeEffect)

data Time :: Effect where
  Now :: Time m UTCTime
  Today :: Time m Day

makeEffect ''Time