{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.TextField where

import Control.Monad
import Control.Monad.State

import Data.Char
import Data.Dynamic
import Data.Typeable

import Debug.Trace

import GUI.Common.Core
import GUI.Common.Drawing
import GUI.Common.Style
import GUI.Data.Tree
import GUI.Widget.Core

import GHC.Generics

import qualified Data.Text as T

data TextFieldState = TextFieldState {
  _tfText :: String,
  _tfPosition :: Int
} deriving (Eq, Show, Typeable, Generic)

textField :: (MonadState s m) => WidgetNode s e m
textField = singleWidget $ makeTextField (TextFieldState "" 0)

{-- 
Check caret logic in nanovg's demo: https://github.com/memononen/nanovg/blob/master/example/demo.c#L901
--}
makeTextField :: (MonadState s m) => TextFieldState -> Widget s e m
makeTextField tfs@(TextFieldState txt tp) = Widget {
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
    (part1, part2) = splitAt tp txt
    printedText = part1 ++ "|" ++ part2
    handleKeyPress currText currTp code
        | isKeyBackspace code && currTp > 0 = (init part1 ++ part2, currTp - 1)
        | isKeyLeft code && currTp > 0 = (currText, currTp - 1)
        | isKeyRight code && currTp < length currText = (currText, currTp + 1)
        | isKeyBackspace code || isKeyLeft code || isKeyRight code = (currText, currTp)
        | length newText > 0 = (part1 ++ newText ++ part2, currTp + length newText)
        | otherwise = (currText, currTp)
      where
        newText = if isKeyPrintable code then [chr code] else ""
        (part1, part2) = splitAt currTp currText
    handleEvent _ evt = case evt of
      KeyAction code KeyPressed -> resultEventsWidget [] (makeTextField newState) where
        (txt2, tp2) = handleKeyPress txt tp code
        newState = TextFieldState txt2 tp2
      _ -> Nothing
    preferredSize renderer (style@Style{..}) _ = do
      size <- calcTextBounds renderer _textStyle (T.pack (if txt == "" then " " else txt))
      return $ SizeReq size FlexibleSize FlexibleSize
    resizeChildren _ _ _ _ = Nothing
    render renderer WidgetInstance{..} _ ts =
      do
        drawBgRect renderer _widgetInstanceRenderArea _widgetInstanceStyle
        drawText renderer _widgetInstanceRenderArea (_textStyle _widgetInstanceStyle) (T.pack printedText)

isKeyPrintable :: KeyCode -> Bool
isKeyPrintable key = key >= 32 && key < 126

isKeyBackspace :: KeyCode -> Bool
isKeyBackspace key = key == 8

isKeyLeft :: KeyCode -> Bool
isKeyLeft key = key == 1073741904

isKeyRight :: KeyCode -> Bool
isKeyRight key = key == 1073741903
