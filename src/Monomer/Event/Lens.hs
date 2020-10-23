{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Event.Lens where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)

import Monomer.Event.Types

makeLensesWith abbreviatedFields ''InputStatus
makeLensesWith abbreviatedFields ''KeyMod
