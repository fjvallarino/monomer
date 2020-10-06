module Monomer.Core.Util where

import Control.Lens ((&), (.~), (?~))
import Data.Text (Text)

import Monomer.Core.Style
import Monomer.Core.WidgetTypes

import qualified Monomer.Core.Lens.Style as S
import qualified Monomer.Core.Lens.Widget as W

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
style inst states = inst & W.style .~ newStyle where
  state = mconcat states
  oldStyle = _wiStyle inst
  newStyle = oldStyle & S.basic ?~ state

hover :: WidgetInstance s e -> [StyleState] -> WidgetInstance s e
hover inst states = inst & W.style .~ newStyle where
  state = mconcat states
  oldStyle = _wiStyle inst
  newStyle = oldStyle & S.hover ?~ state

focus :: WidgetInstance s e -> [StyleState] -> WidgetInstance s e
focus inst states = inst & W.style .~ newStyle where
  state = mconcat states
  oldStyle = _wiStyle inst
  newStyle = oldStyle & S.focus ?~ state

visible :: WidgetInstance s e -> Bool -> WidgetInstance s e
visible widgetInst visibility = widgetInst {
  _wiVisible = visibility
}
