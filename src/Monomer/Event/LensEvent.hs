{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Event.LensEvent where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)

import Monomer.Event.Types

makeLensesWith abbreviatedFields ''InputStatus
