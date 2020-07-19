{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.TextField (
  TextFieldConfig(..),
  textField,
  textField_,
  textFieldConfig
) where

import Control.Monad
import Control.Lens (ALens', (&), (^#), (#~))
import Data.Maybe
import Data.Text (Text)
import Data.Typeable

import qualified Data.Text as T

import Monomer.Common.Geometry
import Monomer.Common.Style
import Monomer.Common.Tree
import Monomer.Event.Core
import Monomer.Event.Keyboard
import Monomer.Event.Types
import Monomer.Graphics.Drawing
import Monomer.Graphics.Types
import Monomer.Widget.BaseWidget
import Monomer.Widget.PathContext
import Monomer.Widget.Types
import Monomer.Widget.Util

data TextFieldConfig s e = TextFieldConfig {
  _tfcValue :: WidgetValue s Text,
  _tfcOnChange :: [Text -> e],
  _tfcOnChangeReq :: [WidgetRequest s],
  _tfcCaretWidth :: Double
}

data TextFieldState = TextFieldState {
  _tfCurrText :: Text,
  _tfPosition :: Int
} deriving (Eq, Show, Typeable)

textFieldConfig :: WidgetValue s Text -> TextFieldConfig s e
textFieldConfig value = TextFieldConfig {
  _tfcValue = value,
  _tfcOnChange = [],
  _tfcOnChangeReq = [],
  _tfcCaretWidth = 2
}

textFieldState :: TextFieldState
textFieldState = TextFieldState {
  _tfCurrText = "",
  _tfPosition = 0
}

textField :: ALens' s Text -> WidgetInstance s e
textField field = textField_ config where
  config = textFieldConfig (WidgetLens field)

textField_ :: TextFieldConfig s e -> WidgetInstance s e
textField_ config = makeInstance $ makeTextField config textFieldState

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = (defaultWidgetInstance "textField" widget) {
  _instanceFocusable = True
}

makeTextField :: TextFieldConfig s e -> TextFieldState -> Widget s e
makeTextField config state = createWidget {
    _widgetInit = init,
    _widgetGetState = makeState state,
    _widgetMerge = widgetMerge merge,
    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetRender = render
  }
  where
    TextFieldState currText currPos = state
    (part1, part2) = T.splitAt currPos currText
    currentValue wenv = widgetValueGet (_weModel wenv) (_tfcValue config)

    init wenv ctx widgetInstance = resultWidget newInstance where
      currText = currentValue wenv
      newState = TextFieldState currText 0
      newInstance = widgetInstance {
        _instanceWidget = makeTextField config newState
      }

    merge wenv ctx oldState widgetInstance = resultWidget newInstance where
      TextFieldState _ oldPos = fromMaybe textFieldState (useState oldState)
      currText = currentValue wenv
      newPos = if | T.length currText < oldPos -> T.length currText
                  | otherwise -> oldPos
      newState = TextFieldState currText newPos
      newInstance = widgetInstance {
        _instanceWidget = makeTextField config newState
      }

    handleKeyPress txt tp code
        | isKeyBackspace code && tp > 0 = (T.append (T.init part1) part2, tp - 1)
        | isKeyLeft code && tp > 0 = (txt, tp - 1)
        | isKeyRight code && tp < T.length txt = (txt, tp + 1)
        | isKeyBackspace code || isKeyLeft code || isKeyRight code = (txt, tp)
        | otherwise = (txt, tp)

    handleEvent wenv ctx evt widgetInstance = case evt of
      Click (Point x y) _ -> Just $ resultReqs reqs widgetInstance where
        reqs = [SetFocus $ currentPath ctx]

      KeyAction mod code KeyPressed -> Just $ resultReqs reqs newInstance where
        (newText, newPos) = handleKeyPress currText currPos code
        reqGetClipboard = [GetClipboard (currentPath ctx) | isClipboardPaste wenv evt]
        reqSetClipboard = [SetClipboard (ClipboardText currText) | isClipboardCopy wenv evt]
        reqUpdateUserState = if | currText /= newText -> widgetValueSet (_tfcValue config) newText
                                | otherwise -> []
        reqs = reqGetClipboard ++ reqSetClipboard ++ reqUpdateUserState
        newState = TextFieldState newText newPos
        newInstance = widgetInstance {
          _instanceWidget = makeTextField config newState
        }

      TextInput newText -> insertText wenv widgetInstance newText

      Clipboard (ClipboardText newText) -> insertText wenv widgetInstance newText

      _ -> Nothing

    insertText wenv widgetInstance addedText = Just $ resultReqs reqs newInstance where
      newText = T.concat [part1, addedText, part2]
      newPos = currPos + T.length addedText
      newState = TextFieldState newText newPos
      reqs = widgetValueSet (_tfcValue config) newText
      newInstance = widgetInstance {
        _instanceWidget = makeTextField config newState
      }
  
    preferredSize wenv widgetInstance = singleNode sizeReq where
      Style{..} = _instanceStyle widgetInstance
      size = getTextBounds wenv _styleText currText
      sizeReq = SizeReq size FlexibleSize StrictSize

    render renderer wenv ctx WidgetInstance{..} =
      let ts = _weTimestamp wenv
          textStyle = _styleText _instanceStyle
          cursorAlpha = if isFocused ctx then fromIntegral (ts `mod` 1000) / 1000.0 else 0
          textColor = (tsTextColor textStyle) { _alpha = cursorAlpha }
          renderArea@(Rect rl rt rw rh) = _instanceRenderArea
      in do
        drawStyledBackground renderer renderArea _instanceStyle
        Rect tl tt _ _ <- drawText renderer renderArea textStyle currText

        when (isFocused ctx) $ do
          let Size sw sh = getTextBounds wenv textStyle part1
          drawRect renderer (Rect (tl + sw) tt (_tfcCaretWidth config) sh) (Just textColor) Nothing
          return ()
