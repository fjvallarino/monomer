{-|
Module      : Monomer.TestEventUtil
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for creating events in unit tests.
-}
module Monomer.TestEventUtil where

import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Text (Text)

import Monomer.Core
import Monomer.Event

import qualified Monomer.Lens as L

-- Tests were written assuming Mac OS is the host
-- For Mac OS, Meta acts as Windows' Ctrl (and viceversa) on text movement/selection

modA :: KeyMod
modA = def
  & L.leftAlt .~ True

modC :: KeyMod
modC = def
  & L.leftCtrl .~ True

modG :: KeyMod
modG = def
  & L.leftGUI .~ True

modS :: KeyMod
modS = def & L.leftShift .~ True

modAS :: KeyMod
modAS = def
  & L.leftAlt .~ True
  & L.leftShift .~ True

modCS :: KeyMod
modCS = def
  & L.leftCtrl .~ True
  & L.leftShift .~ True

modGS :: KeyMod
modGS = def
  & L.leftGUI .~ True
  & L.leftShift .~ True

evtFocus :: SystemEvent
evtFocus = Focus emptyPath

evtBlur :: SystemEvent
evtBlur = Blur emptyPath

evtClick :: Point -> SystemEvent
evtClick p = Click p LeftBtn

evtDblClick :: Point -> SystemEvent
evtDblClick p = DblClick p LeftBtn

evtPress :: Point -> SystemEvent
evtPress p = ButtonAction p LeftBtn PressedBtn 1

evtRelease :: Point -> SystemEvent
evtRelease p = ButtonAction p LeftBtn ReleasedBtn 1

evtReleaseDrag :: Point -> SystemEvent
evtReleaseDrag p = ButtonAction p LeftBtn ReleasedBtn 0

evtMove :: Point -> SystemEvent
evtMove p = Move p

evtDrag :: Point -> Point -> [SystemEvent]
evtDrag start end = [evtPress start, evtMove end, evtReleaseDrag end]

evtK :: KeyCode -> SystemEvent
evtK k = KeyAction def k KeyPressed

evtKA :: KeyCode -> SystemEvent
evtKA k = KeyAction modA k KeyPressed

evtKC :: KeyCode -> SystemEvent
evtKC k = KeyAction modC k KeyPressed

evtKG :: KeyCode -> SystemEvent
evtKG k = KeyAction modG k KeyPressed

evtKS :: KeyCode -> SystemEvent
evtKS k = KeyAction modS k KeyPressed

evtKAS :: KeyCode -> SystemEvent
evtKAS k = KeyAction modAS k KeyPressed

evtKCS :: KeyCode -> SystemEvent
evtKCS k = KeyAction modCS k KeyPressed

evtKGS :: KeyCode -> SystemEvent
evtKGS k = KeyAction modGS k KeyPressed

evtRK :: KeyCode -> SystemEvent
evtRK k = KeyAction def k KeyReleased

evtRKA :: KeyCode -> SystemEvent
evtRKA k = KeyAction modA k KeyReleased

evtRKC :: KeyCode -> SystemEvent
evtRKC k = KeyAction modC k KeyReleased

evtRKG :: KeyCode -> SystemEvent
evtRKG k = KeyAction modG k KeyReleased

evtRKS :: KeyCode -> SystemEvent
evtRKS k = KeyAction modS k KeyReleased

evtRKAS :: KeyCode -> SystemEvent
evtRKAS k = KeyAction modAS k KeyReleased

evtRKCS :: KeyCode -> SystemEvent
evtRKCS k = KeyAction modCS k KeyReleased

evtRKGS :: KeyCode -> SystemEvent
evtRKGS k = KeyAction modGS k KeyReleased

evtT :: Text -> SystemEvent
evtT t = TextInput t

moveCharL :: SystemEvent
moveCharL = evtK keyLeft

moveCharR :: SystemEvent
moveCharR = evtK keyRight

moveWordL :: SystemEvent
moveWordL = evtKA keyLeft

moveWordR :: SystemEvent
moveWordR = evtKA keyRight

moveLineL :: SystemEvent
moveLineL = evtKC keyLeft

moveLineR :: SystemEvent
moveLineR = evtKC keyRight

selCharL :: SystemEvent
selCharL = evtKS keyLeft

selCharR :: SystemEvent
selCharR = evtKS keyRight

selWordL :: SystemEvent
selWordL = evtKAS keyLeft

selWordR :: SystemEvent
selWordR = evtKAS keyRight

selLineL :: SystemEvent
selLineL = evtKCS keyLeft

selLineR :: SystemEvent
selLineR = evtKCS keyRight

delCharL :: SystemEvent
delCharL = evtK keyBackspace

delWordL :: SystemEvent
delWordL = evtKA keyBackspace
