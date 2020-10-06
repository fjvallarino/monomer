{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Core.Lens.Style where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)

-- Imported to avoid having typeclasses created more than once
import Monomer.Core.Lens.Basic
import Monomer.Core.StyleTypes

makeLensesWith abbreviatedFields ''SizeReq
makeLensesWith abbreviatedFields ''Margin
makeLensesWith abbreviatedFields ''Padding
makeLensesWith abbreviatedFields ''BorderSide
makeLensesWith abbreviatedFields ''Border
makeLensesWith abbreviatedFields ''Radius
makeLensesWith abbreviatedFields ''TextStyle
makeLensesWith abbreviatedFields ''StyleState
makeLensesWith abbreviatedFields ''Style
makeLensesWith abbreviatedFields ''ThemeState
makeLensesWith abbreviatedFields ''Theme
