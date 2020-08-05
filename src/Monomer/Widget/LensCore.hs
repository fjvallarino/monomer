{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widget.LensCore where

import Control.Lens.TH (abbreviatedFields, makeLensesWith)

import Monomer.Common.LensStyle
import Monomer.Widget.Types

makeLensesWith abbreviatedFields ''SizePolicy
makeLensesWith abbreviatedFields ''SizeReq
makeLensesWith abbreviatedFields ''WidgetEnv
makeLensesWith abbreviatedFields ''WidgetRequest
makeLensesWith abbreviatedFields ''WidgetResult
makeLensesWith abbreviatedFields ''WidgetPlatform
makeLensesWith abbreviatedFields ''WidgetValue
makeLensesWith abbreviatedFields ''WidgetInstance
