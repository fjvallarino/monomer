{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Core.LensStyle where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)

import Monomer.Core.Style

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
