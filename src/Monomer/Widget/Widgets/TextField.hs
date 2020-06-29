{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.TextField (textField) where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Data.Typeable
import Lens.Micro

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

caretWidth = 2

data TextFieldState = TextFieldState {
  _tfText :: Text,
  _tfPosition :: Int
} deriving (Eq, Show, Typeable)

emptyState = TextFieldState "" 0

textField :: Lens' s Text -> WidgetInstance s e
textField userField = makeInstance $ makeTextField userField emptyState

makeInstance :: Widget s e -> WidgetInstance s e
makeInstance widget = (defaultWidgetInstance "textField" widget) {
  _instanceFocusable = True
}

makeTextField :: Lens' s Text -> TextFieldState -> Widget s e
makeTextField userField tfs@(TextFieldState currText currPos) = createWidget {
    _widgetInit = initTextField,
    _widgetGetState = getState,
    _widgetMerge = widgetMerge merge,

    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetRender = render
  }
  where
    initTextField wctx ctx widgetInstance = resultWidget newInstance where
      app = _wcApp wctx
      newState = TextFieldState (app ^. userField) 0
      newInstance = widgetInstance { _instanceWidget = makeTextField userField newState }
    getState = makeState tfs
    merge wctx oldState = makeTextField userField newState where
      app = _wcApp wctx
      TextFieldState txt pos = fromMaybe emptyState (useState oldState)
      appText = app ^. userField
      newPos = if T.length appText < pos then T.length appText else pos
      newState = TextFieldState appText newPos

    (part1, part2) = T.splitAt currPos currText
    handleKeyPress txt tp code
        | isKeyBackspace code && tp > 0 = (T.append (T.init part1) part2, tp - 1)
        | isKeyLeft code && tp > 0 = (txt, tp - 1)
        | isKeyRight code && tp < T.length txt = (txt, tp + 1)
        | isKeyBackspace code || isKeyLeft code || isKeyRight code = (txt, tp)
        | otherwise = (txt, tp)

    handleEvent wctx ctx evt widgetInstance = case evt of
      Click (Point x y) _ status -> Just $ resultReqs reqs widgetInstance where
        isPressed = status == PressedBtn
        reqs = [SetFocus $ currentPath ctx | isPressed]

      KeyAction mod code KeyPressed -> Just $ resultReqs reqs newInstance where
        (newText, newPos) = handleKeyPress currText currPos code
        reqs = reqGetClipboard ++ reqSetClipboard ++ reqUpdateUserState
        reqGetClipboard = [GetClipboard (currentPath ctx) | isClipboardPaste evt]
        reqSetClipboard = [SetClipboard (ClipboardText currText) | isClipboardCopy evt]
        reqUpdateUserState = [UpdateUserState $ \app -> app & userField .~ newText | currText /= newText]
        newState = TextFieldState newText newPos
        newInstance = widgetInstance { _instanceWidget = makeTextField userField newState }

      TextInput newText -> insertText wctx widgetInstance newText

      Clipboard (ClipboardText newText) -> insertText wctx widgetInstance newText

      _ -> Nothing

    insertText wctx widgetInstance addedText = Just $ resultReqs [UpdateUserState $ \app -> app & userField .~ newText] newInstance where
      newText = T.concat [part1, addedText, part2]
      newPos = currPos + T.length addedText
      newState = TextFieldState newText newPos
      newInstance = widgetInstance { _instanceWidget = makeTextField userField newState }
  
    preferredSize renderer app widgetInstance = singleNode sizeReq where
      Style{..} = _instanceStyle widgetInstance
      size = calcTextBounds renderer _styleText (if currText == "" then " " else currText)
      sizeReq = SizeReq size FlexibleSize FlexibleSize

    render renderer wctx ctx WidgetInstance{..} =
      let ts = _wcTimestamp wctx
          textStyle = _styleText _instanceStyle
          cursorAlpha = if isFocused ctx then fromIntegral (ts `mod` 1000) / 1000.0 else 0
          textColor = (tsTextColor textStyle) { _alpha = cursorAlpha }
          renderArea@(Rect rl rt rw rh) = _instanceRenderArea
      in do
        drawBgRect renderer renderArea _instanceStyle
        Rect tl tt _ _ <- drawText renderer renderArea textStyle currText

        when (isFocused ctx) $ do
          let Size sw sh = calcTextBounds renderer textStyle (if part1 == "" then " " else part1)
          drawRect renderer (Rect (tl + sw) tt caretWidth sh) (Just textColor) Nothing
          return ()
