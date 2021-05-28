{-|
Module      : Monomer
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

This is the package that should be imported by applications using existing
widgets.

If you want to create custom Widgets, check:

- "Monomer.Widgets.Single" for self contained widgets
- "Monomer.Widgets.Container" for widgets with children
-}
module Monomer (
    module Monomer.Core,
    module Monomer.Core.Combinators,
    module Monomer.Core.Themes.SampleThemes,
    module Monomer.Event,
    module Monomer.Graphics,
    module Monomer.Graphics.ColorTable,
    module Monomer.Main,
    module Monomer.Widgets
) where

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Core.Themes.SampleThemes
import Monomer.Event
import Monomer.Graphics
import Monomer.Graphics.ColorTable
import Monomer.Main
import Monomer.Widgets
