{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Widgets where

import Control.Monad.State
import Data.Char
import Data.Default
import Data.Maybe
import Data.Typeable
import Debug.Trace

import GUI.Core
import GUI.Data.Tree
import GUI.Widget.Core
import GUI.Widget.Drawing
import GUI.Widget.Style

import qualified Data.Text as T

container_ :: (Monad m) => [Tree (WidgetNode e m)] -> Tree (WidgetNode e m)
container_ = parentWidget makeContainer

emptyState :: Maybe ()
emptyState = Nothing

makeContainer :: (Monad m) => Widget e m
makeContainer = Widget widgetType handleEvent preferredSize resizeChildren render
  where
    widgetType = "container"
    handleEvent _ _ = NoEvents
    preferredSize _ _ _ = return def
    resizeChildren _ _ children = []
    render _ _ _ _ = return ()

button :: (Monad m) => e -> Tree (WidgetNode e m)
button onClick = singleWidget (makeButton 0 onClick)

makeButton :: (Monad m) => Int -> e -> Widget e m
makeButton state onClick = Widget widgetType handleEvent preferredSize resizeChildren render
  where
    widgetType = "button"
    handleEvent view evt = case evt of
      Click (Point x y) _ status -> EventsState events (makeButton newState onClick) where
        isPressed = status == PressedBtn && inRect view (Point x y)
        newState = if isPressed then state + 1 else state
        events = if isPressed then [onClick] else []
      _ -> NoEvents
    preferredSize renderer (style@Style{..}) _ = calcTextBounds renderer _textStyle (T.pack (show state))
    resizeChildren _ _ _ = []
    render renderer ts viewport (style@Style{..}) =
      do
        drawBgRect renderer viewport style
        drawText renderer viewport _textStyle (T.pack (show state))

data Direction = Horizontal | Vertical deriving (Show, Eq)

hgrid_ :: (Monad m) => [Tree (WidgetNode e m)] -> Tree (WidgetNode e m)
hgrid_ = parentWidget makeHGrid

makeHGrid :: (Monad m) => Widget e m
makeHGrid = makeFixedGrid "hgrid" Horizontal

vgrid_ :: (Monad m) => [Tree (WidgetNode e m)] -> Tree (WidgetNode e m)
vgrid_ = parentWidget makeVGrid

makeVGrid :: (Monad m) => Widget e m
makeVGrid = makeFixedGrid "vgrid" Vertical

makeFixedGrid :: (Monad m) => WidgetType -> Direction -> Widget e m
makeFixedGrid widgetType direction = Widget widgetType handleEvent preferredSize resizeChildren render
  where
    handleEvent _ _ = NoEvents
    render _ _ _ _ = return ()
    preferredSize _ _ children = return $ Size width height where
      width = (fromIntegral wMul) * (maximum . map _w) children
      height = (fromIntegral hMul) * (maximum . map _h) children
      wMul = if direction == Horizontal then length children else 1
      hMul = if direction == Horizontal then 1 else length children
    resizeChildren (Rect l t w h) style children = newWidgets where
      cols = if direction == Horizontal then (length children) else 1
      rows = if direction == Horizontal then 1 else (length children)
      newWidgets = fmap resizeChild [0..(length children - 1)]
      resizeChild i = Rect (cx i) (cy i) cw ch
      cw = w / fromIntegral cols
      ch = h / fromIntegral rows
      cx i = l + (fromIntegral $ i `div` rows) * cw
      cy i = t + (fromIntegral $ i `div` cols) * ch

data TextFieldState = TextFieldState {
  _tfText :: String,
  _tfPosition :: Int
} deriving (Eq, Show)

textField_ :: (Monad m) => Tree (WidgetNode e m)
textField_ = singleWidget $ makeTextField (TextFieldState "" 0)

makeTextField :: (Monad m) => TextFieldState -> Widget e m
makeTextField (TextFieldState txt tp) = Widget widgetType handleEvent preferredSize resizeChildren render
  where
    widgetType = "textField"
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
      KeyAction code KeyPressed -> EventsState [] (makeTextField newState) where
        (txt2, tp2) = handleKeyPress txt tp code
        newState = TextFieldState txt2 tp2
      _ -> NoEvents
    preferredSize renderer (style@Style{..}) _ = calcTextBounds renderer _textStyle (T.pack txt)
    resizeChildren _ _ _ = []
    render renderer ts viewport (style@Style{..}) = do
      drawBgRect renderer viewport style
      drawText renderer viewport _textStyle (T.pack txt)

isKeyPrintable :: KeyCode -> Bool
isKeyPrintable key = key >= 32 && key < 126

isKeyBackspace :: KeyCode -> Bool
isKeyBackspace key = key == 8

isKeyLeft :: KeyCode -> Bool
isKeyLeft key = key == 1073741904

isKeyRight :: KeyCode -> Bool
isKeyRight key = key == 1073741903

{--
makeSizedGrid :: (Monad m) => Direction -> Widget e m
makeSizedGrid direction = Widget widgetType handleEvent preferredSize resizeChildren render
  where
    widgetType = "directionalLayout"
    handleEvent _ _ = NoEvents
    render _ _ _ _ = return ()
    preferredSize _ _ children = return $ Size (width children) (height children) where
      width = if direction == Horizontal then (sum . map _w) else (maximum . (map _w))
      height = if direction == Horizontal then (maximum . (map _h)) else (sum . (map _h))
    resizeChildren rect style children = []
--}

{--
hgrid :: (Monad m) => Rect -> [Widget s m] -> m Bool -> Widget s m
hgrid rect widgets isVisible = makeGrid rect (length widgets) 1 widgets isVisible

vgrid :: (Monad m) => Rect -> [Widget s m] -> m Bool -> Widget s m
vgrid rect widgets isVisible = makeGrid rect 1 (length widgets) widgets isVisible

makeGrid :: (Monad m) => Rect -> Int -> Int -> [Widget s m] -> m Bool -> Widget s m
makeGrid r@(Rect l t w h) rows cols widgets iv = widget
  where
    widget = Widget widgetData iv (handleEvent widgets) (render widgets) resize showMe
    widgetData = WidgetData l t w h
    handleEvent widgets _ e = do
      newWidgets <- mapM (\wt -> _handleEvent wt (Rect l t w h) e) widgets
      pure $ makeGrid r rows cols newWidgets iv
    render widgets r _ = mapM_ (\Widget{..} -> whenM _isVisible $ _render r (widgetDataToRect _widgetData)) widgets
    showMe = show $ fmap _widgetData widgets
    resize _ = makeGrid r rows cols newWidgets iv
      where
        newWidgets = fmap resizeChild (zip [0..] widgets)
        resizeChild (i, child@(Widget {..})) = _resize $ child { _widgetData = (WidgetData (cx i) (cy i) cw ch) }
        cw = w / fromIntegral cols
        ch = h / fromIntegral rows
        cx i = l + (fromIntegral $ i `mod` rows) * cw
        cy i = t + (fromIntegral $ i `div` cols) * ch
--}

{--



labelField :: (MonadState s m) => Rect -> String -> Widget s m
labelField (Rect l t w h) label = widget
  where
    widget = Widget widgetData (pure True) handleEvent render resize undefined
    widgetData = WidgetData l t w h
    handleEvent _ _ = pure widget
    render r rt@(Rect x y w h) = do
      fillColor r (RGB 255 0 0)
      text r rt "sans" 32 (Align Center Middle) (T.pack label)
    resize w@Widget{..} = labelField (widgetDataToRect _widgetData) label



--}
