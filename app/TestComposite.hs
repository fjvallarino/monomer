{-# LANGUAGE TemplateHaskell #-}

module TestComposite (testComposite) where

import Debug.Trace

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad (forM_)

import Data.Default
import Data.Typeable (Typeable)
import TextShow

import Monomer.Common.Style
import Monomer.Common.StyleUtil
import Monomer.Graphics.Color
import Monomer.Main.Util
import Monomer.Widget.CompositeWidget
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widgets

import Types

data CompState = CompState {
  _csCounter :: Int,
  _csProduced :: Int
} deriving (Show, Eq)

instance Default CompState where
  def = CompState 0 0

makeLenses ''CompState

data CompEvent = InitComposite
               | MessageParent
               | CallSandbox
               | StartTask
               | StartProducer
               | HandleProducer Int
               deriving (Eq, Show)

--testComposite :: WidgetInstance sp AppEvent
testComposite = composite "testComposite" def (Just InitComposite) handleCompositeEvent buildComposite

--handleCompositeEvent :: CompState -> CompEvent -> EventResponse CompState CompEvent AppEvent
handleCompositeEvent model evt = case evt of
  InitComposite -> Task $ do
    threadDelay 1000
    putStrLn "Initialized composite"
    return Nothing
  MessageParent -> Report IncreaseMessage
  CallSandbox -> Event (HandleProducer 20) <> (Task $ return Nothing)
  StartTask -> Task $ do
    putStrLn "Composite event handler called"
    return Nothing
  StartProducer -> Producer $ \sendMessage ->
    forM_ [1..10] $ \_ -> do
      sendMessage (HandleProducer 1)
      threadDelay $ 1000 * 1000
  HandleProducer val -> Model $ model & csProduced %~ (+val)

buildComposite model = trace "Created composite UI" $
  vgrid [
    scroll $ label "This is a composite label!",
    scroll $ label "This is a composite label again!",
    vgrid [
      hgrid [
        button MessageParent "Message parent"
      ],
      hgrid [
        sandbox CallSandbox,
        button StartTask "Run task"
      ],
      hgrid [
        button StartProducer "Run Producer",
        label ("Produced: " <> showt (_csProduced model))
      ]
    ] `style` color gray
  ]
