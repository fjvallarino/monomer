{-|
Module      : Monomer.Widgets.Containers.Base.LabeledItem
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Container for items with an associated clickable label. Mainly used with radio
and checkbox.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Monomer.Widgets.Containers.Base.LabeledItem (
  labeledItem
) where

import Control.Applicative ((<|>))
import Data.Default
import Control.Lens ((&), (^.), (^?), (^?!), (.~), (<>~), ix)
import Data.Maybe
import Data.Sequence ((|>))
import Data.Text (Text)

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators

import Monomer.Widgets.Container
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.Spacer

import qualified Monomer.Lens as L

labeledItem
  :: WidgetEvent e
  => WidgetType
  -> RectSide
  -> Text
  -> LabelCfg
  -> WidgetNode s e
  -> WidgetNode s e
labeledItem wtype textSide caption labelCfg itemNode = labeledNode where
  widget = makeLabeledItem textSide caption labelCfg itemNode
  labeledNode = defaultWidgetNode wtype widget

makeLabeledItem
  :: WidgetEvent e
  => RectSide
  -> Text
  -> LabelCfg
  -> WidgetNode s e
  -> Widget s e
makeLabeledItem textSide caption labelCfg itemNode = widget where
  widget = createContainer () def {
    containerInit = init,
    containerMerge = merge,
    containerFilterEvent = filterEvent
  }

  createChildNode wenv node = newNode where
    nodeStyle = node ^. L.info . L.style
    labelStyle = def
      & collectStyleField_ L.text nodeStyle
      & collectStyleField_ L.cursorIcon nodeStyle
    itemStyle = def
      & collectStyleField_ L.fgColor nodeStyle
      & collectStyleField_ L.hlColor nodeStyle
      & collectStyleField_ L.sndColor nodeStyle
      & collectStyleField_ L.cursorIcon nodeStyle
    baseLabel = label_ caption [labelCfg] `style` [cursorHand]
    labelNode = baseLabel
      & L.info . L.style <>~ labelStyle
    styledNode = itemNode
      & L.info . L.style <>~ itemStyle
    childNode
      | textSide == SideLeft = hstack [ labelNode, spacer, styledNode ]
      | textSide == SideRight = hstack [ styledNode, spacer, labelNode ]
      | textSide == SideTop = vstack [ labelNode, spacer, styledNode ]
      | otherwise = vstack [ styledNode, spacer, labelNode ]
    newNode = node
      & L.children .~ Seq.singleton childNode

  init wenv node = result where
    result = resultNode (createChildNode wenv node)

  merge wenv node oldNode oldState = result where
    result = resultNode (createChildNode wenv node)

  filterEvent :: ContainerFilterHandler s e
  filterEvent wenv node target evt = case evt of
    Click p btn clicks
      | isPointInNodeVp p labelNode -> Just (newPath, newEvt) where
        newEvt = Click targetCenter btn clicks
    ButtonAction p btn BtnPressed clicks
      | isPointInNodeVp p labelNode -> Just (newPath, newEvt) where
        newEvt = ButtonAction targetCenter btn BtnPressed clicks
    _ -> Just (target, evt)
    where
      labelIdx
        | textSide `elem` [SideLeft, SideTop] = 0
        | otherwise = 2
      targetIdx = 2 - labelIdx
      newPath = Seq.take (length target - 1) target |> targetIdx
      labelNode = node ^. L.children . ix 0 . L.children ^?! ix labelIdx
      targetNode = node ^. L.children . ix 0 . L.children ^?! ix targetIdx
      targetCenter = rectCenter (targetNode ^. L.info . L.viewport)
