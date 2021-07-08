{-|
Module      : Monomer.Core.Lens
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Lenses for the Core types.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Core.Lens where

import Control.Lens.TH (abbreviatedFields, makeLensesWith, makePrisms)

import Monomer.Common.Lens
import Monomer.Core.StyleTypes
import Monomer.Core.ThemeTypes
import Monomer.Core.WidgetTypes

-- Style
makeLensesWith abbreviatedFields ''SizeReq
makeLensesWith abbreviatedFields ''Padding
makeLensesWith abbreviatedFields ''BorderSide
makeLensesWith abbreviatedFields ''Border
makeLensesWith abbreviatedFields ''Radius
makeLensesWith abbreviatedFields ''RadiusCorner
makeLensesWith abbreviatedFields ''TextStyle
makeLensesWith abbreviatedFields ''StyleState
makeLensesWith abbreviatedFields ''Style
makeLensesWith abbreviatedFields ''ThemeState
makeLensesWith abbreviatedFields ''Theme

-- Widget
makePrisms ''WidgetKey
makePrisms ''WidgetData
makePrisms ''WidgetType
makeLensesWith abbreviatedFields ''WidgetEnv
makeLensesWith abbreviatedFields ''WidgetRequest
makeLensesWith abbreviatedFields ''WidgetResult
makeLensesWith abbreviatedFields ''WidgetData
makeLensesWith abbreviatedFields ''WidgetId
makeLensesWith abbreviatedFields ''WidgetNode
makeLensesWith abbreviatedFields ''WidgetNodeInfo
makeLensesWith abbreviatedFields ''WidgetInstanceNode
