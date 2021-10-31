{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module IsolatedComposite (
  isolatedComposite
) where

import Control.Lens

import qualified Data.Text as T

import Monomer

import qualified Monomer.Lens as L

type IsolatedWenv = WidgetEnv IsolatedModel IsolatedEvt
type IsolatedNode = WidgetNode IsolatedModel IsolatedEvt

newtype IsolatedModel = IsolatedModel {
  _clickCount :: Int
} deriving (Eq, Show)

data IsolatedEvt
  = IsolatedInc

makeLenses ''IsolatedModel

isolatedComposite :: (WidgetModel s, WidgetEvent e) => WidgetNode s e
isolatedComposite = compositeD_ "isolated" wdata buildUI handleEvent cfgs where
  state = IsolatedModel 0
  wdata = WidgetValue state
  mergeModel parent old new = old
  cfgs = [customModelBuilder mergeModel]

buildUI :: IsolatedWenv -> IsolatedModel -> IsolatedNode
buildUI wenv model = widgetTree where
  row idx = label (T.pack $ "Row " ++ show idx)
    `styleBasic` [paddingV 50]
  rows = row <$> [1..model ^. clickCount]
  widgetTree = vstack [
      button "Increase" IsolatedInc,
      vscroll (vstack rows)
    ]

handleEvent :: IsolatedWenv -> IsolatedNode -> IsolatedModel -> IsolatedEvt -> [EventResponse IsolatedModel IsolatedEvt sp ep]
handleEvent wenv node model evt = case evt of
  IsolatedInc -> [
      Model $ model & clickCount %~ (+1)
    ]
