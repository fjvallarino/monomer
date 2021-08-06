{-|
Module      : Monomer
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Main module, re-exporting all the modules of the library. This is the module
that should be imported by applications.

If you want to create custom Widgets, check:

- "Monomer.Widgets.Single" for self contained widgets
- "Monomer.Widgets.Container" for widgets with children

If you don't want to use all the helper modules, or you don't want to import
them unqualified, the basic modules you will need are:

- "Monomer.Common"
- "Monomer.Core"
- "Monomer.Event"
- "Monomer.Graphics"
- "Monomer.Main"
- "Monomer.Widgets"
-}
module Monomer (
    module Monomer.Common,
    module Monomer.Core,
    module Monomer.Core.Combinators,
    module Monomer.Core.Themes.SampleThemes,
    module Monomer.Event,
    module Monomer.Graphics,
    module Monomer.Graphics.ColorTable,
    module Monomer.Graphics.RemixIcon,
    module Monomer.Main,
    module Monomer.Widgets
) where

import Monomer.Common
import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Core.Themes.SampleThemes
import Monomer.Event
import Monomer.Graphics
import Monomer.Graphics.ColorTable
import Monomer.Graphics.RemixIcon
import Monomer.Main
import Monomer.Widgets
