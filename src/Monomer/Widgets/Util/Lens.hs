{-|
Module      : Monomer.Widgets.Util.Lens
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Lenses for the Widget types.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Util.Lens where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)

import Monomer.Core.Lens
import Monomer.Widgets.Util.Types

makeLensesWith abbreviatedFields ''ActiveStyleCfg
