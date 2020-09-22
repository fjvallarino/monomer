{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widget.Widgets.Box (
  BoxConfig,
  box,
  box_
) where

import Data.Default

import qualified Data.Sequence as Seq

import Monomer.Event.Types
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.BaseContainer
import Monomer.Widget.Widgets.WidgetCombinators

data BoxConfig s e = BoxConfig {
  _boxOnClick :: [e],
  _boxOnClickReq :: [WidgetRequest s]
}

instance Default (BoxConfig s e) where
  def = BoxConfig {
    _boxOnClick = [],
    _boxOnClickReq = []
  }

instance Semigroup (BoxConfig s e) where
  (<>) t1 t2 = BoxConfig {
    _boxOnClick = _boxOnClick t1 <> _boxOnClick t2,
    _boxOnClickReq = _boxOnClickReq t1 <> _boxOnClickReq t2
  }

instance Monoid (BoxConfig s e) where
  mempty = def

instance OnClick (BoxConfig s e) e where
  onClick handler = def {
    _boxOnClick = [handler]
  }

instance OnClickReq (BoxConfig s e) s where
  onClickReq req = def {
    _boxOnClickReq = [req]
  }

box :: BoxConfig s e -> WidgetInstance s e -> WidgetInstance s e
box config managed = box_ managed config

box_ :: WidgetInstance s e -> BoxConfig s e -> WidgetInstance s e
box_ managed config = makeInstance (makeBox config) managed

makeInstance :: Widget s e -> WidgetInstance s e -> WidgetInstance s e
makeInstance widget managedWidget = (defaultWidgetInstance "box" widget) {
  _wiChildren = Seq.singleton managedWidget,
  _wiFocusable = False
}

makeBox :: BoxConfig s e -> Widget s e
makeBox config = widget where
  widget = createContainer def {
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  handleEvent wenv ctx evt widgetInst = case evt of
    Click point btn -> result where
      events = _boxOnClick config
      requests = _boxOnClickReq config
      needsUpdate = btn == LeftBtn && not (null events && null requests)
      result
        | needsUpdate = Just $ resultReqsEvents requests events widgetInst
        | otherwise = Nothing
    _ -> Nothing

  getSizeReq wenv widgetInst children = sizeReq where
    sizeReq = _wiSizeReq $ Seq.index children 0

  resize wenv viewport renderArea children widgetInst = resized where
    assignedArea = Seq.singleton (viewport, renderArea)
    resized = (widgetInst, assignedArea)
