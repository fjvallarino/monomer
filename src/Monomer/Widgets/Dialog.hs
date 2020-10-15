module Monomer.Widgets.Dialog where

import Control.Lens ((&), (.~))
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Typeable (Typeable)

import Monomer.Core
import Monomer.Event
import Monomer.Graphics

import Monomer.Widgets.Box
import Monomer.Widgets.Button
import Monomer.Widgets.Label
import Monomer.Widgets.Stack

import qualified Monomer.Lens as L

--newtype DialogCfg e = DialogCfg {
--  _dgcButtons :: [(Text, e)]
--}

alert :: Text -> Text -> Text -> e -> WidgetInstance s e
alert title message caption evt = alertBox where
  alertTree = vstack [
      label title `style` [textLeft],
      label message `style` [rangeHeight 100 200],
      hstack [
        button caption evt
      ]
    ] `style` [bgColor gray]
  alertBox = box_ alertTree [onClick evt] `style` [bgColor $ darkGray & L.a .~ 0.8]

makeConfirm :: Text -> Text -> WidgetInstance sd ed
makeConfirm title message = vstack [
    label title,
    label message
  ]

--makeDialog
--  :: (Eq sd, Typeable sd, Typeable ed)
--  => WidgetType
--  -> sd
--  -> WidgetInstance sd ed
--  -> WidgetInstance s e
--makeDialog widgetType model body = comp where
--  buildUI _ = body
--  comp = composite widgetType model Nothing handleEvent buildUI
--
--handleEvent :: s -> e -> EventResponse s e ep
--handleEvent model evt = Model model
