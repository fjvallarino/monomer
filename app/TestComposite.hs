{-# LANGUAGE TemplateHaskell #-}

module TestComposite (testComp) where

import Debug.Trace

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad (forM_)

import Data.Default
import TextShow

import Monomer.Core.Combinators
import Monomer.Core.Style
import Monomer.Graphics.Color
import Monomer.Graphics.ColorTable (gray)
import Monomer.Core.WidgetTypes
import Monomer.Core.Util
import Monomer.Widgets

import Types

data CompState = CompState {
  _csCounter :: Int,
  _csProduced :: Int
} deriving (Eq, Show)

instance Default CompState where
  def = CompState 0 0

makeLenses ''CompState

data CompEvent
  = InitComp
  | IgnoreCompEvt
  | MessageParent
  | CallSandbox
  | StartTask
  | StartProducer
  | HandleProducer Int
  deriving (Eq, Show)

testComp :: WidgetNode CompState AppEvent
testComp = composite_ "testComp" id buildComp handleCompEvt [onInit InitComp]

handleCompEvt wenv node model evt = case evt of
  InitComp -> [Task $ do
    threadDelay 1000
    putStrLn "Initialized composite"
    return IgnoreCompEvt]
  IgnoreCompEvt -> []
  MessageParent -> [Report IncreaseMessage]
  CallSandbox -> [Event (HandleProducer 20)]
  StartTask -> [Task $ do
    putStrLn "Composite event handler called"
    return IgnoreCompEvt]
  StartProducer -> [Producer $ \sendMessage ->
    forM_ [1..10] $ \_ -> do
      sendMessage (HandleProducer 1)
      threadDelay $ 1000 * 1000]
  HandleProducer val -> [Model $ model & csProduced %~ (+val)]

buildComp wenv model = trace "Created composite UI" $
  vgrid [
    scroll $ label "This is a composite label!",
    scroll $ label "This is a composite label again!",
    vgrid [
      hgrid [
        button "Message parent" MessageParent
      ],
      hgrid [
        button "Run task" StartTask
      ],
      hgrid [
        button "Run Producer" StartProducer,
        label ("Produced: " <> showt (_csProduced model))
      ]
    ] `style` [bgColor gray]
  ]
