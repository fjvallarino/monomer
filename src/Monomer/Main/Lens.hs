{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Main.Lens where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)

import Monomer.Core.Lens
import Monomer.Main.Types

makeLensesWith abbreviatedFields ''AppConfig
makeLensesWith abbreviatedFields ''MonomerCtx
makeLensesWith abbreviatedFields ''MonomerCtxPersist
makeLensesWith abbreviatedFields ''RenderSchedule
