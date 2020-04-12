{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.TextField where

import Control.Monad
import Control.Monad.State

import Data.Char
import Data.Dynamic
import Data.Maybe
import Data.Typeable
import qualified Data.Text as T

import GUI.Common.Core
import GUI.Common.Event
import GUI.Common.Drawing
import GUI.Common.Keyboard
import GUI.Common.Style
import GUI.Common.Types
import GUI.Common.Util
import GUI.Data.Tree

import GHC.Generics

import Lens.Micro
import Lens.Micro.Mtl

caretWidth = 2

data TextFieldState = TextFieldState {
  _tfText :: T.Text,
  _tfPosition :: Int
} deriving (Eq, Show, Typeable, Generic)

emptyState = TextFieldState "" 0

textField :: (MonadState s m) => Lens' s T.Text -> WidgetNode s e m
textField userField = singleWidget $ makeTextField userField emptyState

{-- 
Check caret logic in nanovg's demo: https://github.com/memononen/nanovg/blob/master/example/demo.c#L901
--}
makeTextField :: (MonadState s m) => Lens' s T.Text -> TextFieldState -> Widget s e m
makeTextField userField tfs@(TextFieldState currText currPos) = Widget {
    _widgetType = "textField",
    _widgetFocusable = True,
    _widgetRestoreState = restoreState,
    _widgetSaveState = makeState tfs,
    _widgetUpdateUserState = updateUserState,
    _widgetHandleEvent = handleEvent,
    _widgetHandleCustom = defaultCustomHandler,
    _widgetPreferredSize = preferredSize,
    _widgetResizeChildren = resizeChildren,
    _widgetRender = render
  }
  where
    (part1, part2) = T.splitAt currPos currText
    handleKeyPress txt tp code
        | isKeyBackspace code && tp > 0 = (T.append (T.init part1) part2, tp - 1)
        | isKeyLeft code && tp > 0 = (txt, tp - 1)
        | isKeyRight code && tp < T.length txt = (txt, tp + 1)
        | isKeyBackspace code || isKeyLeft code || isKeyRight code = (txt, tp)
        | otherwise = (txt, tp)
    restoreState app st = if appText /= currText then newWidget else Nothing where
      TextFieldState txt pos = fromMaybe emptyState (useState st)
      appText = app ^. userField
      newPos = if T.length appText > pos then T.length appText else pos
      newWidget = Just $ makeTextField userField (TextFieldState appText newPos)
    updateUserState = userField .= currText
    handleEvent _ evt = case evt of
      KeyAction mod code KeyPressed -> resultReqsEventsWidget reqs [] (makeTextField userField newState) where
        (newText, newPos) = handleKeyPress currText currPos code
        reqs = reqGetClipboard ++ reqSetClipboard ++ reqUpdateUserState
        reqGetClipboard = if isClipboardPaste evt then [GetClipboard] else []
        reqSetClipboard = if isClipboardCopy evt then [SetClipboard (ClipboardText currText)] else []
        reqUpdateUserState = if currText /= newText then [UpdateUserState] else []
        newState = TextFieldState newText newPos
      TextInput newText -> insertText newText
      Clipboard (ClipboardText newText) -> insertText newText
      _ -> Nothing
    insertText addedText = resultReqsEventsWidget [UpdateUserState] [] (makeTextField userField newState) where
      newText = T.concat [part1, addedText, part2]
      newPos = currPos + T.length addedText
      newState = TextFieldState newText newPos
    preferredSize renderer (style@Style{..}) _ = do
      size <- calcTextBounds renderer _textStyle (if currText == "" then " " else currText)
      return $ sizeReq size FlexibleSize FlexibleSize
    resizeChildren _ _ _ _ = Nothing
    render renderer WidgetInstance{..} _ ts =
      let textStyle = _textStyle _widgetInstanceStyle
          cursorAlpha = if _widgetInstanceFocused then (fromIntegral $ ts `mod` 1000) / 1000.0 else 0
          textColor = (tsTextColor textStyle) { _alpha = cursorAlpha }
          renderArea@(Rect rl rt rw rh) = _widgetInstanceRenderArea
      in do
        drawBgRect renderer renderArea _widgetInstanceStyle
        Rect tl tt _ _ <- drawText renderer renderArea textStyle currText

        when True $ do
          Size sw sh <- calcTextBounds renderer textStyle (if part1 == "" then " " else part1)
          drawRect renderer (Rect (tl + sw) tt caretWidth sh) (Just textColor) Nothing
          return ()
