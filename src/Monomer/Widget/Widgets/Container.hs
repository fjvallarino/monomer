{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.Container (
  ContainerConfig(..),
  container,
  containerConfig
) where

import Control.Monad (when)
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

data ContainerConfig s e = ContainerConfig {
  _ctOnClick :: [e],
  _ctOnClickReq :: [WidgetRequest s]
}

containerConfig :: ContainerConfig s e
containerConfig = ContainerConfig [] []

container :: ContainerConfig s e -> WidgetInstance s e -> WidgetInstance s e
container config managed = makeInstance (makeContainer config) managed

makeInstance :: Widget s e -> WidgetInstance s e -> WidgetInstance s e
makeInstance widget managedWidget = (defaultWidgetInstance "container" widget) {
  _instanceChildren = Seq.singleton managedWidget,
  _instanceFocusable = False
}

makeContainer :: ContainerConfig s e -> Widget s e
makeContainer config = widget where
  widget = createContainer {
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = containerResize resize,
    _widgetRender = containerRender render
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

  preferredSize wenv widgetInst children reqs = Node sizeReq reqs where
    sizeReq = nodeValue $ Seq.index reqs 0

  resize wenv viewport renderArea widgetInst children reqs = resized where
    assignedArea = Seq.singleton (viewport, renderArea)
    resized = (widgetInst, assignedArea)

  render renderer wenv widgetInst = do
    drawRect renderer viewport _styleColor Nothing

    when (pointInRect point viewport) $
      drawRect renderer viewport _styleHover Nothing

    where
      point = statusMousePos (_weInputStatus wenv)
      viewport = _instanceViewport widgetInst
      Style{..} = _instanceStyle widgetInst
