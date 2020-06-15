{-# LANGUAGE TemplateHaskell #-}

module KeysComposite (keysComposite) where

import Debug.Trace

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)

import Data.Default
import Data.Sequence (Seq(..), (|>))
import Data.Text (Text)
import Data.Typeable (Typeable)
--import Lens.Micro
import Lens.Micro.GHC
import Lens.Micro.TH (makeLenses)
import TextShow

import qualified Data.Sequence as Seq

import Monomer.Common.Style
import Monomer.Graphics.Color
import Monomer.Main.Util
import Monomer.Widget.CompositeWidget
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widgets

data EditableItem = EditableItem {
  _eiId :: Text,
  _eiText :: Text
} deriving (Show, Eq)

data KeysCompState = KeysCompState {
  _kcsItems :: Seq EditableItem
} deriving (Show, Eq)

makeLenses ''EditableItem
makeLenses ''KeysCompState

data KeysCompEvent = RotateChildren
               deriving (Eq, Show)

initialState = KeysCompState {
  _kcsItems = Seq.fromList [
    EditableItem "1" "Text 1",
    EditableItem "2" "Text 2",
    EditableItem "3" "Text 3",
    EditableItem "4" "Text 4",
    EditableItem "5" "Text 5"
  ]
}

keysComposite = composite "keysComposite" initialState Nothing handleKeysCompEvent buildKeysComp

handleKeysCompEvent :: KeysCompState -> KeysCompEvent -> EventResponseC KeysCompState KeysCompEvent ep
handleKeysCompEvent app evt = case evt of
  RotateChildren -> StateC (app & kcsItems %~ rotateSeq)

buildKeysComp app = trace "Created keys composite UI" $
  hgrid [
    button "Add new" RotateChildren,
    vgrid $ fmap (editableItem app) [0..(length (_kcsItems app) - 1)]
  ]

editableItem app idx = hgrid [
    label "Enter text!",
    textField (singular $ kcsItems . ix idx . eiText)
  ] `key` (app ^. (singular $ kcsItems . ix idx . eiId))

rotateSeq Empty = Empty
rotateSeq (x :<| xs) = xs |> x
