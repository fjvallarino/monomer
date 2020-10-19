{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Box (
  BoxCfg,
  box,
  box_,
  expandContent
) where

import Control.Applicative ((<|>))
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container

data BoxCfg s e = BoxCfg {
  _boxExpandContent :: Maybe Bool,
  _boxAlignH :: Maybe AlignH,
  _boxAlignV :: Maybe AlignV,
  _boxOnClick :: [e],
  _boxOnClickReq :: [WidgetRequest s],
  _boxOnClickEmpty :: [e],
  _boxOnClickEmptyReq :: [WidgetRequest s]
}

instance Default (BoxCfg s e) where
  def = BoxCfg {
    _boxExpandContent = Nothing,
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
    _boxAlignH = _boxAlignH t2 <|> _boxAlignH t1,
    _boxAlignV = _boxAlignV t2 <|> _boxAlignV t1,
    _boxOnClick = _boxOnClick t1 <> _boxOnClick t2,
    _boxOnClickReq = _boxOnClickReq t1 <> _boxOnClickReq t2,
    _boxOnClickEmpty = _boxOnClickEmpty t1 <> _boxOnClickEmpty t2,
    _boxOnClickEmptyReq = _boxOnClickEmptyReq t1 <> _boxOnClickEmptyReq t2
  }

instance Monoid (BoxCfg s e) where
  mempty = def

instance AlignLeft (BoxCfg s e) where
  alignLeft = def {
    _boxAlignH = Just ALeft
  }

instance AlignCenter (BoxCfg s e) where
  alignCenter = def {
    _boxAlignH = Just ACenter
  }

instance AlignRight (BoxCfg s e) where
  alignRight = def {
    _boxAlignH = Just ARight
  }

instance AlignTop (BoxCfg s e) where
  alignTop = def {
    _boxAlignV = Just ATop
  }

instance AlignMiddle (BoxCfg s e) where
  alignMiddle = def {
    _boxAlignV = Just AMiddle
  }

instance AlignBottom (BoxCfg s e) where
  alignBottom = def {
    _boxAlignV = Just ABottom
  }

instance OnClick (BoxCfg s e) e where
  onClick handler = def {
    _boxOnClick = [handler]
  }

instance OnClickReq (BoxCfg s e) s where
  onClickReq req = def {
    _boxOnClickReq = [req]
  }

instance OnClickEmpty (BoxCfg s e) e where
  onClickEmpty handler = def {
    _boxOnClickEmpty = [handler]
  }

instance OnClickEmptyReq (BoxCfg s e) s where
  onClickEmptyReq req = def {
    _boxOnClickEmptyReq = [req]
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

  handleEvent wenv ctx evt inst = case evt of
    Click point btn -> result where
      child = Seq.index (_wiChildren inst) 0
      childClicked = pointInRect point (_wiViewport child)
      events
        | childClicked = _boxOnClick config
        | otherwise = _boxOnClickEmpty config
      requests
        | childClicked  = _boxOnClickReq config
        | otherwise = _boxOnClickEmptyReq config
      needsUpdate = btn == LeftBtn && not (null events && null requests)
      result
        | needsUpdate = Just $ resultReqsEvents requests events inst
        | otherwise = Nothing
    _ -> Nothing

  getSizeReq wenv inst children = (newReqW, newReqH) where
    child = Seq.index children 0
    newReqW = _wiSizeReqW child
    newReqH = _wiSizeReqH child

  resize wenv viewport renderArea children inst = resized where
    Rect vx vy vw vh = viewport
    Rect rx ry rw rh = renderArea
    child = Seq.index children 0
    contentW = getMaxSizeReq $ _wiSizeReqW child
    contentH = getMaxSizeReq $ _wiSizeReqH child
    vpChild = Rect vx vy (min vw contentW) (min vh contentH)
    raChild = Rect rx ry (min rw contentW) (min rh contentH)
    ah = fromMaybe ACenter (_boxAlignH config)
    av = fromMaybe AMiddle (_boxAlignV config)
    vpAligned = alignInRect ah av viewport vpChild
    raAligned = alignInRect ah av renderArea raChild
    expand = fromMaybe False (_boxExpandContent config)
    resized
      | expand = (inst, Seq.singleton (viewport, renderArea))
      | otherwise  = (inst, Seq.singleton (vpAligned, raAligned))

alignInRect :: AlignH -> AlignV -> Rect -> Rect -> Rect
alignInRect ah av parent child = newRect where
  tempRect = alignVInRect av parent child
  newRect = alignHInRect ah parent tempRect

alignHInRect :: AlignH -> Rect -> Rect -> Rect
alignHInRect ah parent child = newRect where
  Rect px _ pw _ = parent
  Rect cx cy cw ch = child
  newX = case ah of
    ALeft -> px
    ACenter -> px + (pw - cw) / 2
    ARight -> px + pw - cw
  newRect = Rect newX cy cw ch

alignVInRect :: AlignV -> Rect -> Rect -> Rect
alignVInRect av parent child = newRect where
  Rect _ py _ ph = parent
  Rect cx cy cw ch = child
  newY = case av of
    ATop -> py
    AMiddle -> py + (ph - ch) / 2
    ABottom -> py + ph - ch
  newRect = Rect cx newY cw ch
