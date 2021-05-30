{-|
Module      : Monomer.Graphics.Lens
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Lenses for the Graphics types.
-}
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
makeLensesWith abbreviatedFields ''ImageDef
makeLensesWith abbreviatedFields ''TextMetrics
makeLensesWith abbreviatedFields ''TextLine
