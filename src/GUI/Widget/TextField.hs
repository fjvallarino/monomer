{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.TextField where

import Control.Monad
import Control.Monad.State

import Data.Char

import Debug.Trace

import GUI.Common.Core
import GUI.Common.Drawing
import GUI.Common.Style
import GUI.Data.Tree
import GUI.Widget.Core

import qualified Data.Text as T

data TextFieldState = TextFieldState {
  _tfText :: String,
  _tfPosition :: Int
} deriving (Eq, Show)

textField :: (MonadState s m) => Tree (WidgetInstance s e m)
textField = singleWidget $ makeTextField (TextFieldState "" 0)

{-- 
Check caret logic in nanovg's demo: https://github.com/memononen/nanovg/blob/master/example/demo.c#L901
--}
makeTextField :: (MonadState s m) => TextFieldState -> Widget s e m
makeTextField (TextFieldState txt tp) = Widget widgetType modifiesContext focusable handleEvent preferredSize resizeChildren render
  where
    widgetType = "textField"
    modifiesContext = False
    focusable = True
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
      KeyAction code KeyPressed -> widgetEventResult False [] (makeTextField newState) where
        (txt2, tp2) = handleKeyPress txt tp code
        newState = TextFieldState txt2 tp2
      _ -> Nothing
    preferredSize renderer (style@Style{..}) _ = calcTextBounds renderer _textStyle (T.pack txt)
    resizeChildren _ _ _ = []
    render renderer viewport (style@Style{..}) status ts = do
      drawBgRect renderer viewport style
      drawText renderer viewport _textStyle (T.pack printedText)

isKeyPrintable :: KeyCode -> Bool
isKeyPrintable key = key >= 32 && key < 126

isKeyBackspace :: KeyCode -> Bool
isKeyBackspace key = key == 8

isKeyLeft :: KeyCode -> Bool
isKeyLeft key = key == 1073741904

isKeyRight :: KeyCode -> Bool
isKeyRight key = key == 1073741903
