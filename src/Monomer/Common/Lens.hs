{-|
Module      : Monomer.Common.Lens
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Lenses for the Common types.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Common.Lens where

import Control.Lens.TH (abbreviatedFields, makeLensesWith, makePrisms)

import Monomer.Common.BasicTypes

-- Basic
makeLensesWith abbreviatedFields ''Point
makeLensesWith abbreviatedFields ''Size
makeLensesWith abbreviatedFields ''Rect
