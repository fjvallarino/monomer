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

import Control.Lens (lens)
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Text (Text)

import Monomer.Common.Lens
import Monomer.Core.Lens
import Monomer.Graphics.Types

makeLensesWith abbreviatedFields ''Color
makeLensesWith abbreviatedFields ''FontDef
makeLensesWith abbreviatedFields ''GlyphPos
makeLensesWith abbreviatedFields ''ImageDef
makeLensesWith abbreviatedFields ''TextMetrics
makeLensesWith abbreviatedFields ''TextLine

instance HasName FontDef Text where
  name = lens getName setName
    where
      getName (FontDefFile name _) = name
      getName (FontDefMem name _) = name
      setName (FontDefFile _ path) name = FontDefFile name path
      setName (FontDefMem _ bytes) name = FontDefMem name bytes
