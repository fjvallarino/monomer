{-# LANGUAGE TemplateHaskell #-}

module TestComposite (testComposite) where

import Debug.Trace

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)

import Data.Default
import Data.Typeable (Typeable)
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import TextShow

import Monomer.Common.Style
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

data CompEvent = Initialize
               | MessageParent
               | CallSandbox
               | RunTask
               | RunProducer
               | HandleProducer Int
               deriving (Eq, Show)

--testComposite :: WidgetInstance sp AppEvent
testComposite = composite "testComposite" def (Just Initialize) handleCompositeEvent buildComposite

--handleCompositeEvent :: CompState -> CompEvent -> EventResponseC CompState CompEvent AppEvent
handleCompositeEvent app evt = case evt of
  Initialize -> EventC RunProducer
  MessageParent -> MessageC IncreaseMessage
  CallSandbox -> EventC (HandleProducer 20) <> (TaskC $ return Nothing)
  RunTask -> TaskC $ do
    putStrLn $ "Composite event handler called"
    return Nothing
  RunProducer -> ProducerC $ \sendMessage -> do
    forM_ [1..10] $ \_ -> do
      sendMessage (HandleProducer 1)
      threadDelay $ 1000 * 1000
  HandleProducer val -> StateC $ app & csProduced %~ (+val)

buildComposite app = trace "Created composite UI" $
  vgrid [
    scroll $ label "This is a composite label!",
    scroll $ label "This is a composite label again!",
    vgrid [
      hgrid [
        button "Message parent" MessageParent
      ],
      hgrid [
        sandbox CallSandbox,
        button "Run task" RunTask
      ],
      hgrid [
        button "Run Producer" RunProducer,
        label ("Produced: " <> (showt $ _csProduced app))
      ]
    ] `style` bgColor gray
  ]
