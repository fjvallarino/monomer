{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Core.Lens.Widget where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)

import Monomer.Core.Lens.Style
import Monomer.Core.WidgetTypes

makeLensesWith abbreviatedFields ''WidgetEnv
makeLensesWith abbreviatedFields ''WidgetRequest
makeLensesWith abbreviatedFields ''WidgetResult
makeLensesWith abbreviatedFields ''WidgetData
makeLensesWith abbreviatedFields ''WidgetInstance
