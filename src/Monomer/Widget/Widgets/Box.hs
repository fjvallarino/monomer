module Monomer.Widget.Widgets.Box (
  BoxConfig(..),
  box,
  boxConfig
) where

import Control.Monad (when)
import Data.Default
import Data.Maybe
import Data.Sequence (Seq(..), (|>))

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Event.Types
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.BaseContainer

data BoxConfig s e = BoxConfig {
  _ctOnClick :: [e],
  _ctOnClickReq :: [WidgetRequest s]
}

boxConfig :: BoxConfig s e
boxConfig = BoxConfig [] []

box :: BoxConfig s e -> WidgetInstance s e -> WidgetInstance s e
box config managed = makeInstance (makeBox config) managed

makeInstance :: Widget s e -> WidgetInstance s e -> WidgetInstance s e
makeInstance widget managedWidget = (defaultWidgetInstance "box" widget) {
  _wiChildren = Seq.singleton managedWidget,
  _wiFocusable = False
}

makeBox :: BoxConfig s e -> Widget s e
makeBox config = widget where
  widget = createContainer def {
    containerHandleEvent = handleEvent,
    containerUpdateSizeReq = updateSizeReq,
    containerResize = resize
  }

  handleEvent wenv ctx evt widgetInst = case evt of
    Click point btn -> result where
      events = _ctOnClick config
      requests = _ctOnClickReq config
      needsUpdate = btn == LeftBtn && not (null events && null requests)
      result
        | needsUpdate = Just $ resultReqsEvents requests events widgetInst
        | otherwise = Nothing
    _ -> Nothing

  updateSizeReq wenv widgetInst children = sizeReq where
    sizeReq = _wiSizeReq $ Seq.index children 0

  resize wenv viewport renderArea children widgetInst = resized where
    assignedArea = Seq.singleton (viewport, renderArea)
    resized = (widgetInst, assignedArea)
