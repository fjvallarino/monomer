module Monomer.Core.Util where

import Control.Lens ((&), (.~), (?~))
import Data.Text (Text)

import Monomer.Core.Style
import Monomer.Core.WidgetTypes

import qualified Monomer.Core.Lens as L

infixl 5 `key`
infixl 5 `style`
infixl 5 `hover`
infixl 5 `focus`
infixl 5 `visible`

key :: WidgetInstance s e -> Text -> WidgetInstance s e
key widgetInst key = widgetInst {
  _wiKey = Just (WidgetKey key)
}

style :: WidgetInstance s e -> [StyleState] -> WidgetInstance s e
style inst states = inst & L.style .~ newStyle where
  state = mconcat states
  oldStyle = _wiStyle inst
  newStyle = oldStyle & L.basic ?~ state

hover :: WidgetInstance s e -> [StyleState] -> WidgetInstance s e
hover inst states = inst & L.style .~ newStyle where
  state = mconcat states
  oldStyle = _wiStyle inst
  newStyle = oldStyle & L.hover ?~ state

focus :: WidgetInstance s e -> [StyleState] -> WidgetInstance s e
focus inst states = inst & L.style .~ newStyle where
  state = mconcat states
  oldStyle = _wiStyle inst
  newStyle = oldStyle & L.focus ?~ state

visible :: WidgetInstance s e -> Bool -> WidgetInstance s e
visible widgetInst visibility = widgetInst {
  _wiVisible = visibility
}
