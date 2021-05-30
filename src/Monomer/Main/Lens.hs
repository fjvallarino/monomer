{-|
Module      : Monomer.Main.Lens
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Lenses for the Main types.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Main.Lens where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)

import Monomer.Core.Lens
import Monomer.Main.Types

makeLensesWith abbreviatedFields ''AppConfig
makeLensesWith abbreviatedFields ''MonomerCtx
makeLensesWith abbreviatedFields ''DragAction
makeLensesWith abbreviatedFields ''RenderSchedule
