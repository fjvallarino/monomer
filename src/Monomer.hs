{-|
Module      : Monomer
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Main module, re-exporting all the modules of the library. This is the module
that should be imported by applications.

To start using the library it is recommended to check the
<https://github.com/fjvallarino/monomer#documentation tutorials>:

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
    -- * Basic types common to all modules.
    module Monomer.Common,
    -- * Core types and utilities.
    module Monomer.Core,
    -- * User friendly names for configuration options.
    module Monomer.Core.Combinators,
    -- * Default theme, with light and dark versions.
    module Monomer.Core.Themes.SampleThemes,
    -- * High level and low level events and utilities.
    module Monomer.Event,
    -- * Graphics types, utilities and renderer implementation.
    module Monomer.Graphics,
    -- * Names for common colors.
    module Monomer.Graphics.ColorTable,
    -- * Icons from the Remix library.
    module Monomer.Graphics.RemixIcon,
    -- * Application launcher.
    module Monomer.Main,
    -- * Available widgets in the library.
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
