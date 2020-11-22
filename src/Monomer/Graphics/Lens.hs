{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Graphics.Lens where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)

import Monomer.Core.Lens
import Monomer.Graphics.Types

makeLensesWith abbreviatedFields ''Color
makeLensesWith abbreviatedFields ''FontDef
makeLensesWith abbreviatedFields ''GlyphPos
makeLensesWith abbreviatedFields ''TextMetrics
makeLensesWith abbreviatedFields ''TextLine
