{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widget.Widgets.Box (
  BoxCfg,
  expandContent,
  box,
  box_
) where

import Control.Applicative ((<|>))
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Common.Geometry
import Monomer.Event.Types
import Monomer.Widget.BaseContainer
import Monomer.Widget.Internal
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widget.Widgets.WidgetCombinators

data BoxCfg s e = BoxCfg {
  _boxExpandContent :: Maybe Bool,
  _boxOnClick :: [e],
  _boxOnClickReq :: [WidgetRequest s]
}

instance Default (BoxCfg s e) where
  def = BoxCfg {
    _boxExpandContent = Nothing,
    _boxOnClick = [],
    _boxOnClickReq = []
  }

instance Semigroup (BoxCfg s e) where
  (<>) t1 t2 = BoxCfg {
    _boxExpandContent = _boxExpandContent t2 <|> _boxExpandContent t1,
    _boxOnClick = _boxOnClick t1 <> _boxOnClick t2,
    _boxOnClickReq = _boxOnClickReq t1 <> _boxOnClickReq t2
  }

instance Monoid (BoxCfg s e) where
  mempty = def

instance OnClick (BoxCfg s e) e where
  onClick handler = def {
    _boxOnClick = [handler]
  }

instance OnClickReq (BoxCfg s e) s where
  onClickReq req = def {
    _boxOnClickReq = [req]
  }

expandContent :: BoxCfg s e
expandContent = def {
  _boxExpandContent = Just True
}

box :: WidgetInstance s e -> WidgetInstance s e
box managed = box_ managed def

box_ :: WidgetInstance s e -> [BoxCfg s e] -> WidgetInstance s e
box_ managed configs = makeInstance (makeBox config) managed where
  config = mconcat configs

makeInstance :: Widget s e -> WidgetInstance s e -> WidgetInstance s e
makeInstance widget managedWidget = (defaultWidgetInstance "box" widget) {
  _wiChildren = Seq.singleton managedWidget,
  _wiFocusable = False
}

makeBox :: BoxCfg s e -> Widget s e
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

  getSizeReq wenv widgetInst children = (newReqW, newReqH) where
    child = Seq.index children 0
    newReqW = _wiSizeReqW child
    newReqH = _wiSizeReqH child

  resize wenv viewport renderArea children widgetInst = resized where
    Rect vx vy vw vh = viewport
    Rect rx ry rw rh = renderArea
    child = Seq.index children 0
    contentW = getMaxReqCoord $ _wiSizeReqW child
    contentH = getMaxReqCoord $ _wiSizeReqH child
    viewportC = Rect vx vy (min vw contentW) (min vh contentH)
    renderAreaC = Rect rx ry (min rw contentW) (min rh contentH)
    expand = fromMaybe False (_boxExpandContent config)
    resized
      | expand = (widgetInst, Seq.singleton (viewport, renderArea))
      | otherwise  = (widgetInst, Seq.singleton (viewportC, renderAreaC))
