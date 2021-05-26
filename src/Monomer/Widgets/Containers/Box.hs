{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widgets.Containers.Box (
  BoxCfg(..),
  box,
  box_,
  expandContent
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container
import Monomer.Widgets.Containers.Stack

import qualified Monomer.Lens as L

data BoxCfg s e = BoxCfg {
  _boxExpandContent :: Maybe Bool,
  _boxIgnoreEmptyArea :: Maybe Bool,
  _boxSizeReqUpdater :: Maybe SizeReqUpdater,
  _boxAlignH :: Maybe AlignH,
  _boxAlignV :: Maybe AlignV,
  _boxOnClick :: [e],
  _boxOnClickReq :: [WidgetRequest s e],
  _boxOnClickEmpty :: [e],
  _boxOnClickEmptyReq :: [WidgetRequest s e]
}

instance Default (BoxCfg s e) where
  def = BoxCfg {
    _boxExpandContent = Nothing,
    _boxIgnoreEmptyArea = Nothing,
    _boxSizeReqUpdater = Nothing,
    _boxAlignH = Nothing,
    _boxAlignV = Nothing,
    _boxOnClick = [],
    _boxOnClickReq = [],
    _boxOnClickEmpty = [],
    _boxOnClickEmptyReq = []
  }

instance Semigroup (BoxCfg s e) where
  (<>) t1 t2 = BoxCfg {
    _boxExpandContent = _boxExpandContent t2 <|> _boxExpandContent t1,
    _boxIgnoreEmptyArea = _boxIgnoreEmptyArea t2 <|> _boxIgnoreEmptyArea t1,
    _boxSizeReqUpdater = _boxSizeReqUpdater t2 <|> _boxSizeReqUpdater t1,
    _boxAlignH = _boxAlignH t2 <|> _boxAlignH t1,
    _boxAlignV = _boxAlignV t2 <|> _boxAlignV t1,
    _boxOnClick = _boxOnClick t1 <> _boxOnClick t2,
    _boxOnClickReq = _boxOnClickReq t1 <> _boxOnClickReq t2,
    _boxOnClickEmpty = _boxOnClickEmpty t1 <> _boxOnClickEmpty t2,
    _boxOnClickEmptyReq = _boxOnClickEmptyReq t1 <> _boxOnClickEmptyReq t2
  }

instance Monoid (BoxCfg s e) where
  mempty = def

instance CmbIgnoreEmptyArea (BoxCfg s e) where
  ignoreEmptyArea_ ignore = def {
    _boxIgnoreEmptyArea = Just ignore
  }

instance CmbSizeReqUpdater (BoxCfg s e) where
  sizeReqUpdater updater = def {
    _boxSizeReqUpdater = Just updater
  }

instance CmbAlignLeft (BoxCfg s e) where
  alignLeft_ False = def
  alignLeft_ True = def {
    _boxAlignH = Just ALeft
  }

instance CmbAlignCenter (BoxCfg s e) where
  alignCenter_ False = def
  alignCenter_ True = def {
    _boxAlignH = Just ACenter
  }

instance CmbAlignRight (BoxCfg s e) where
  alignRight_ False = def
  alignRight_ True = def {
    _boxAlignH = Just ARight
  }

instance CmbAlignTop (BoxCfg s e) where
  alignTop_ False = def
  alignTop_ True = def {
    _boxAlignV = Just ATop
  }

instance CmbAlignMiddle (BoxCfg s e) where
  alignMiddle_ False = def
  alignMiddle_ True = def {
    _boxAlignV = Just AMiddle
  }

instance CmbAlignBottom (BoxCfg s e) where
  alignBottom_ False = def
  alignBottom_ True = def {
    _boxAlignV = Just ABottom
  }

instance CmbOnClick (BoxCfg s e) e where
  onClick handler = def {
    _boxOnClick = [handler]
  }

instance CmbOnClickReq (BoxCfg s e) s e where
  onClickReq req = def {
    _boxOnClickReq = [req]
  }

instance CmbOnClickEmpty (BoxCfg s e) e where
  onClickEmpty handler = def {
    _boxOnClickEmpty = [handler]
  }

instance CmbOnClickEmptyReq (BoxCfg s e) s e where
  onClickEmptyReq req = def {
    _boxOnClickEmptyReq = [req]
  }

expandContent :: BoxCfg s e
expandContent = def {
  _boxExpandContent = Just True
}

box :: WidgetEvent e => WidgetNode s e -> WidgetNode s e
box managed = box_ def managed

box_ :: WidgetEvent e => [BoxCfg s e] -> WidgetNode s e -> WidgetNode s e
box_ configs managed = makeNode (makeBox config) managed where
  config = mconcat configs

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "box" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeBox :: WidgetEvent e => BoxCfg s e -> Widget s e
makeBox config = widget where
  widget = createContainer () def {
    containerIgnoreEmptyArea = ignoreEmptyArea && emptyHandlersCount == 0,
    containerGetActiveStyle = getActiveStyle,
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  ignoreEmptyArea = Just True == _boxIgnoreEmptyArea config
  emptyHandlersCount
    = length (_boxOnClickEmpty config)
    + length (_boxOnClickEmptyReq config)

  getActiveStyle = activeStyle_ activeStyleConfig where
    activeStyleConfig = def
      & L.isActive .~ isNodeTreeActive

  handleEvent wenv node target evt = case evt of
    Click point btn -> result where
      child = Seq.index (node ^. L.children) 0
      childClicked = pointInRect point (child ^. L.info . L.viewport)
      events
        | childClicked = _boxOnClick config
        | otherwise = _boxOnClickEmpty config
      requests
        | childClicked  = _boxOnClickReq config
        | otherwise = _boxOnClickEmptyReq config
      needsUpdate = not (null events && null requests)
      result
        | needsUpdate = Just $ resultReqsEvts node requests events
        | otherwise = Nothing
    _ -> Nothing

  getSizeReq :: ContainerGetSizeReqHandler s e
  getSizeReq wenv node children = newSizeReq where
    updateSizeReq = fromMaybe id (_boxSizeReqUpdater config)
    child = Seq.index children 0
    newReqW = child ^. L.info . L.sizeReqW
    newReqH = child ^. L.info . L.sizeReqH
    newSizeReq = updateSizeReq (newReqW, newReqH)

  resize :: ContainerResizeHandler s e
  resize wenv node viewport children = resized where
    style = getActiveStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style viewport)
    Rect cx cy cw ch = contentArea
    child = Seq.index children 0
    contentW = snd $ assignStackAreas True contentArea children
    contentH = snd $ assignStackAreas False contentArea children
    raChild = Rect cx cy (min cw contentW) (min ch contentH)
    ah = fromMaybe ACenter (_boxAlignH config)
    av = fromMaybe AMiddle (_boxAlignV config)
    raAligned = alignInRect ah av contentArea raChild
    expand = fromMaybe False (_boxExpandContent config)
    resized
      | expand = (resultNode node, Seq.singleton contentArea)
      | otherwise = (resultNode node, Seq.singleton raAligned)

alignInRect :: AlignH -> AlignV -> Rect -> Rect -> Rect
alignInRect ah av parent child = newRect where
  tempRect = alignVInRect av parent child
  newRect = alignHInRect ah parent tempRect

alignHInRect :: AlignH -> Rect -> Rect -> Rect
alignHInRect ah parent child = newRect where
  Rect px _ pw _ = parent
  Rect _ cy cw ch = child
  newX = case ah of
    ALeft -> px
    ACenter -> px + (pw - cw) / 2
    ARight -> px + pw - cw
  newRect = Rect newX cy cw ch

alignVInRect :: AlignV -> Rect -> Rect -> Rect
alignVInRect av parent child = newRect where
  Rect _ py _ ph = parent
  Rect cx _ cw ch = child
  newY = case av of
    ATop -> py
    AMiddle -> py + (ph - ch) / 2
    ABottom -> py + ph - ch
  newRect = Rect cx newY cw ch
