module TestComposite where

import Debug.Trace

import Control.Monad.State
import Data.Default
import Data.Typeable (Typeable)
import Lens.Micro
import TextShow

import Monomer.Common.Style
import Monomer.Graphics.Color
import Monomer.Widget.CompositeWidget
import Monomer.Widget.Types
import Monomer.Widget.Util
import Monomer.Widgets

import Types

data CompEvent = CEvent1
               | CEvent2
               | CEvent3
               | CEvent4
               deriving (Eq, Show)

testComposite :: (Monad m, Typeable m) => WidgetInstance sp AppEvent m
testComposite = composite "testComposite" def handleCompositeEvent buildComposite

handleCompositeEvent :: CompState -> CompEvent -> EventResponseC CompState CompEvent AppEvent
handleCompositeEvent app evt = case evt of
  CEvent1 -> StateC $ app & csCounter %~ (+1)
  CEvent2 -> MessageC (IncreaseCount 55)
  CEvent3 -> TaskC app $ return Nothing
  otherwise -> TaskC app $ do
    liftIO . putStrLn $ "Composite event handler called"
    return $ Just CEvent1

buildComposite app = trace "Created composite UI" $
  vstack [
    scroll $ label "This is a composite label!",
    vgrid [
      hgrid [
        button ("Clicked: " <> (showt $ _csCounter app)) CEvent1,
        button "Message parent" CEvent2
      ],
      hgrid [
        sandbox CEvent3,
        button "Run task" CEvent4
      ]
    ] `style` bgColor gray
  ]
