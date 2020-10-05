{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Core.Lens.Basic where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)

import Monomer.Core.BasicTypes
import Monomer.Core.Types

makeLensesWith abbreviatedFields ''Point
makeLensesWith abbreviatedFields ''Size
makeLensesWith abbreviatedFields ''Rect
