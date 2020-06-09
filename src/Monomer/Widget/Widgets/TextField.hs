{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Widget.Widgets.TextField (textField) where

import Control.Monad
import Data.Maybe
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
  _tfText :: T.Text,
  _tfPosition :: Int
} deriving (Eq, Show, Typeable)

emptyState = TextFieldState "" 0

textField :: (Monad m) => Lens' s T.Text -> WidgetInstance s e m
textField userField = makeInstance $ makeTextField userField emptyState

makeInstance :: (Monad m) => Widget s e m -> WidgetInstance s e m
makeInstance widget = (defaultWidgetInstance "textField" widget) {
  _instanceFocusable = True
}

makeTextField :: (Monad m) => Lens' s T.Text -> TextFieldState -> Widget s e m
makeTextField userField tfs@(TextFieldState currText currPos) = createWidget {
    _widgetGetState = getState,
    _widgetMerge = widgetMerge merge,

    _widgetHandleEvent = handleEvent,
    _widgetPreferredSize = preferredSize,
    _widgetRender = render
  }
  where
    getState = makeState tfs
    merge app oldState = makeTextField userField newState where
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

    handleEvent ctx evt app widgetInstance = case evt of
      Click (Point x y) _ status -> resultReqs reqs widgetInstance where
        isPressed = status == PressedBtn
        reqs = if isPressed then [SetFocus $ currentPath ctx] else []

      KeyAction mod code KeyPressed -> resultReqs reqs newInstance where
        (newText, newPos) = handleKeyPress currText currPos code
        reqs = reqGetClipboard ++ reqSetClipboard ++ reqUpdateUserState
        reqGetClipboard = if isClipboardPaste evt then [GetClipboard (currentPath ctx)] else []
        reqSetClipboard = if isClipboardCopy evt then [SetClipboard (ClipboardText currText)] else []
        reqUpdateUserState = if currText /= newText then [UpdateUserState $ \app -> app & userField .~ newText] else []
        newState = TextFieldState newText newPos
        newInstance = widgetInstance { _instanceWidget = makeTextField userField newState }

      TextInput newText -> insertText app widgetInstance newText

      Clipboard (ClipboardText newText) -> insertText app widgetInstance newText

      _ -> Nothing

    insertText app widgetInstance addedText = resultReqs [UpdateUserState $ \app -> app & userField .~ newText] newInstance where
      newText = T.concat [part1, addedText, part2]
      newPos = currPos + T.length addedText
      newState = TextFieldState newText newPos
      newInstance = widgetInstance { _instanceWidget = makeTextField userField newState }
  
    preferredSize renderer app widgetInstance = singleNode sizeReq where
      Style{..} = _instanceStyle widgetInstance
      size = calcTextBounds renderer _textStyle (if currText == "" then " " else currText)
      sizeReq = SizeReq size FlexibleSize FlexibleSize

    render renderer ts ctx app WidgetInstance{..} =
      let textStyle = _textStyle _instanceStyle
          cursorAlpha = if isFocused ctx then (fromIntegral $ ts `mod` 1000) / 1000.0 else 0
          textColor = (tsTextColor textStyle) { _alpha = cursorAlpha }
          renderArea@(Rect rl rt rw rh) = _instanceRenderArea
      in do
        drawBgRect renderer renderArea _instanceStyle
        Rect tl tt _ _ <- drawText renderer renderArea textStyle currText

        when True $ do
          let Size sw sh = calcTextBounds renderer textStyle (if part1 == "" then " " else part1)
          drawRect renderer (Rect (tl + sw) tt caretWidth sh) (Just textColor) Nothing
          return ()
