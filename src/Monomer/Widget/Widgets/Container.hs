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
container config managedWidget = makeInstance (makeContainer config) managedWidget

makeInstance :: Widget s e -> WidgetInstance s e -> WidgetInstance s e
makeInstance widget managedWidget = (defaultWidgetInstance "container" widget) {
  _instanceChildren = Seq.singleton managedWidget,
  _instanceFocusable = False
}

makeContainer :: ContainerConfig s e -> Widget s e
makeContainer config = createContainer {
    _widgetHandleEvent = containerHandleEvent handleEvent,
    _widgetPreferredSize = containerPreferredSize preferredSize,
    _widgetResize = containerResize resize,
    _widgetRender = containerRender render
  }
  where
    handleEvent wenv ctx evt widgetInstance = case evt of
      Click point btn -> result where
        events = _ctOnClick config
        requests = _ctOnClickReq config
        result = if btn == LeftBtn && not (null events && null requests)
                    then Just $ resultReqsEvents requests events widgetInstance
                    else Nothing
      _ -> Nothing

    preferredSize wenv widgetInstance children reqs = Node sizeReq reqs where
      sizeReq = nodeValue $ Seq.index reqs 0

    resize wenv viewport renderArea widgetInstance children reqs = (widgetInstance, assignedArea) where
      assignedArea = Seq.singleton (viewport, renderArea)

    render renderer wenv ctx widgetInstance = do
      let point = statusMousePos (_weInputStatus wenv)
      let viewport = _instanceViewport widgetInstance
      let Style{..} = _instanceStyle widgetInstance

      drawRect renderer viewport _styleColor Nothing

      when (pointInRect point viewport) $
        drawRect renderer viewport _styleHover Nothing
