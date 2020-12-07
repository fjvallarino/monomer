{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Core.Lens where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)

import Monomer.Core.BasicTypes
import Monomer.Core.StyleTypes
import Monomer.Core.ThemeTypes
import Monomer.Core.WidgetTypes

-- Basic
makeLensesWith abbreviatedFields ''Point
makeLensesWith abbreviatedFields ''Size
makeLensesWith abbreviatedFields ''Rect

-- Style
makeLensesWith abbreviatedFields ''SizeReq
makeLensesWith abbreviatedFields ''Padding
makeLensesWith abbreviatedFields ''BorderSide
makeLensesWith abbreviatedFields ''Border
makeLensesWith abbreviatedFields ''Radius
makeLensesWith abbreviatedFields ''TextStyle
makeLensesWith abbreviatedFields ''StyleState
makeLensesWith abbreviatedFields ''Style
makeLensesWith abbreviatedFields ''ThemeState
makeLensesWith abbreviatedFields ''Theme

-- Widget
makeLensesWith abbreviatedFields ''WidgetEnv
makeLensesWith abbreviatedFields ''WidgetRequest
makeLensesWith abbreviatedFields ''WidgetResult
makeLensesWith abbreviatedFields ''WidgetSizeReq
makeLensesWith abbreviatedFields ''WidgetData
makeLensesWith abbreviatedFields ''WidgetNode
makeLensesWith abbreviatedFields ''WidgetInstance
makeLensesWith abbreviatedFields ''WidgetInstanceNode
