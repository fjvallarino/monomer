module Monomer.TestEventUtil where

import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Text (Text)

import Monomer.Core
import Monomer.Event

import qualified Monomer.Lens as L

-- For Mac OS, Meta acts as Windows' Ctrl (and viceversa) on text movement/selection
modA :: KeyMod
modA = def
  & L.leftCtrl .~ True

modC :: KeyMod
modC = def
  & L.leftAlt .~ True

modG :: KeyMod
modG = def
  & L.leftGUI .~ True

modS :: KeyMod
modS = def & L.leftShift .~ True

modAS :: KeyMod
modAS = def
  & L.leftCtrl .~ True
  & L.leftShift .~ True

modCS :: KeyMod
modCS = def
  & L.leftAlt .~ True
  & L.leftShift .~ True

modGS :: KeyMod
modGS = def
  & L.leftGUI .~ True
  & L.leftShift .~ True

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

evtT :: Text -> SystemEvent
evtT t = TextInput t

moveCharL :: SystemEvent
moveCharL = evtK keyLeft

moveCharR :: SystemEvent
moveCharR = evtK keyRight

moveWordL :: SystemEvent
moveWordL = evtKC keyLeft

moveWordR :: SystemEvent
moveWordR = evtKC keyRight

moveLineL :: SystemEvent
moveLineL = evtKA keyLeft

moveLineR :: SystemEvent
moveLineR = evtKA keyRight

selCharL :: SystemEvent
selCharL = evtKS keyLeft

selCharR :: SystemEvent
selCharR = evtKS keyRight

selWordL :: SystemEvent
selWordL = evtKCS keyLeft

selWordR :: SystemEvent
selWordR = evtKCS keyRight

selLineL :: SystemEvent
selLineL = evtKAS keyLeft

selLineR :: SystemEvent
selLineR = evtKAS keyRight

delCharL :: SystemEvent
delCharL = evtK keyBackspace

delWordL :: SystemEvent
delWordL = evtKC keyBackspace
