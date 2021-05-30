{-|
Module      : Monomer.Event.Lens
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Lenses for the Event types.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Event.Lens where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)

import Monomer.Event.Types

makeLensesWith abbreviatedFields ''InputStatus
makeLensesWith abbreviatedFields ''KeyMod
