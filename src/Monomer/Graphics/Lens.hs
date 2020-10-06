{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Graphics.Lens where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)

import Monomer.Graphics.Types

makeLensesWith abbreviatedFields ''Color
