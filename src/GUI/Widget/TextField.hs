{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.TextField where

import Control.Monad
import Control.Monad.State

import Data.Char
import Data.Dynamic
import Data.Typeable

import GUI.Common.Core
import GUI.Common.Event
import GUI.Common.Drawing
import GUI.Common.Keyboard
import GUI.Common.Style
import GUI.Common.Types
import GUI.Data.Tree

import GHC.Generics

import qualified Data.Text as T

data TextFieldState = TextFieldState {
  _tfText :: T.Text,
  _tfPosition :: Int
} deriving (Eq, Show, Typeable, Generic)

textField :: (MonadState s m) => WidgetNode s e m
textField = singleWidget $ makeTextField (TextFieldState "" 0)

{-- 
Check caret logic in nanovg's demo: https://github.com/memononen/nanovg/blob/master/example/demo.c#L901
--}
makeTextField :: (MonadState s m) => TextFieldState -> Widget s e m
makeTextField tfs@(TextFieldState currText currPos) = Widget {
    _widgetType = "textField",
    _widgetFocusable = True,
    _widgetRestoreState = fmap makeTextField . useState,
    _widgetSaveState = makeState tfs,
    _widgetHandleEvent = handleEvent,
    _widgetHandleCustom = defaultCustomHandler,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render
  }
  where
    (part1, part2) = T.splitAt currPos currText
    printedText = T.concat [part1, "|", part2]
    handleKeyPress currText currTp code
        | isKeyBackspace code && currTp > 0 = (T.append (T.init part1) part2, currTp - 1)
        | isKeyLeft code && currTp > 0 = (currText, currTp - 1)
        | isKeyRight code && currTp < T.length currText = (currText, currTp + 1)
        | isKeyBackspace code || isKeyLeft code || isKeyRight code = (currText, currTp)
        | otherwise = (currText, currTp)
      where
        (part1, part2) = T.splitAt currTp currText
    handleEvent _ evt = case evt of
      KeyAction mod code KeyPressed -> resultReqsEventsWidget reqs [] (makeTextField newState) where
        (newText, newPos) = handleKeyPress currText currPos code
        reqs = reqGetClipboard ++ reqSetClipboard
        reqGetClipboard = if isClipboardPaste evt then [GetClipboard] else []
        reqSetClipboard = if isClipboardCopy evt then [SetClipboard (ClipboardText currText)] else []
        newState = TextFieldState newText newPos
      TextInput newText -> insertText newText
      Clipboard (ClipboardText newText) -> insertText newText
      _ -> Nothing
    insertText addedText = resultEventsWidget [] (makeTextField newState) where
      newText = T.concat [part1, addedText, part2]
      newPos = currPos + T.length addedText
      newState = TextFieldState newText newPos
    preferredSize renderer (style@Style{..}) _ = do
      size <- calcTextBounds renderer _textStyle (if currText == "" then " " else currText)
      return $ SizeReq size FlexibleSize FlexibleSize
    resizeChildren _ _ _ _ = Nothing
    render renderer WidgetInstance{..} _ ts =
      do
        drawBgRect renderer _widgetInstanceRenderArea _widgetInstanceStyle
        drawText renderer _widgetInstanceRenderArea (_textStyle _widgetInstanceStyle) printedText
