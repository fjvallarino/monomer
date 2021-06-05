{-|
Module      : Monomer.Event
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Event module, including all related types and utility functions. Based on SDL.
-}
module Monomer.Event (
  module Monomer.Event.Core,
  module Monomer.Event.Keyboard,
  module Monomer.Event.Types,
  module Monomer.Event.Util
) where

import Monomer.Event.Core
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Event.Util
