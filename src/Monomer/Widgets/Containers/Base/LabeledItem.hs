{-|
Module      : Monomer.Widgets.Containers.Base.LabeledItem
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Container for items with an associated clickable label. Mainly used with radio
and checkbox.

For usage examples, see:

- "Monomer.Widgets.Singles.LabeledCheckbox"
- "Monomer.Widgets.Singles.LabeledRadio"
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
import Monomer.Core.Combinators as Cmb

import Monomer.Widgets.Container
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.Spacer

import qualified Monomer.Lens as L

{-|
Creates a stack with a label and the provided widget, passing to this widget all
the click events received. Positioning is configurable.
-}
labeledItem
  :: WidgetEvent e
  => WidgetType
  -> RectSide
  -> Maybe Double
  -> Text
  -> LabelCfg s e
  -> WidgetNode s e
  -> WidgetNode s e
labeledItem wtype textSide childSpacing caption labelCfg itemNode = labeledNode where
  widget = makeLabeledItem textSide childSpacing caption labelCfg itemNode
  labeledNode = defaultWidgetNode wtype widget

makeLabeledItem
  :: WidgetEvent e
  => RectSide
  -> Maybe Double
  -> Text
  -> LabelCfg s e
  -> WidgetNode s e
  -> Widget s e
makeLabeledItem textSide childSpacing caption labelCfg itemNode = widget where
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

    baseLabel = label_ caption [labelCfg] `styleBasic` [cursorHand]
    labelNode = baseLabel
      & L.info . L.style <>~ labelStyle
    styledNode = itemNode
      & L.info . L.style <>~ itemStyle

    childNode
      | textSide == SideLeft = hstack_ stackCfg [ labelNode, styledNode ]
      | textSide == SideRight = hstack_ stackCfg [ styledNode, labelNode ]
      | textSide == SideTop = vstack_ stackCfg [ labelNode, styledNode ]
      | otherwise = vstack_ stackCfg [ styledNode, labelNode ]
    stackCfg =
      [maybe Cmb.childSpacing Cmb.childSpacing_ childSpacing]
    newNode = node
      & L.children .~ Seq.singleton childNode

  init wenv node = result where
    result = resultNode (createChildNode wenv node)

  merge wenv node oldNode oldState = result where
    result = resultNode (createChildNode wenv node)

  filterEvent :: ContainerFilterHandler s e
  filterEvent wenv node target evt = case evt of
    Click p btn clicks
      | isPointInNodeVp labelNode p -> Just (newPath, newEvt) where
        newEvt = Click targetCenter btn clicks

    ButtonAction p btn BtnPressed clicks
      | isPointInNodeVp labelNode p -> Just (newPath, newEvt) where
        newEvt = ButtonAction targetCenter btn BtnPressed clicks

    _ -> Just (target, evt)
    where
      labelIdx
        | textSide `elem` [SideLeft, SideTop] = 0
        | otherwise = 1
      targetIdx = 1 - labelIdx
      newPath = Seq.take (length target - 1) target |> targetIdx
      labelNode = node ^. L.children . ix 0 . L.children ^?! ix labelIdx
      targetNode = node ^. L.children . ix 0 . L.children ^?! ix targetIdx
      targetCenter = rectCenter (targetNode ^. L.info . L.viewport)
