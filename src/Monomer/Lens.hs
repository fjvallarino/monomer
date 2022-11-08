{-|
Module      : Monomer.Lens
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Module grouping all the different lens modules. Useful import to avoid duplicate
instance errors.
-}
module Monomer.Lens (
  module Monomer.Common.Lens,
  module Monomer.Core.Lens,
  module Monomer.Event.Lens,
  module Monomer.Graphics.Lens,
  module Monomer.Main.Lens,
  module Monomer.Widgets.Util.Lens
) where

import Monomer.Common.Lens
import Monomer.Core.Lens
import Monomer.Event.Lens
import Monomer.Graphics.Lens
import Monomer.Main.Lens
import Monomer.Widgets.Util.Lens
