{-# LANGUAGE RecordWildCards #-}

module GUI.Widget.Widgets where

import Control.Monad.State
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
      resizeChild i = traceShow i $ Rect (cx i) (cy i) cw ch
      cw = w / fromIntegral cols
      ch = h / fromIntegral rows
      cx i = l + (fromIntegral $ i `div` rows) * cw
      cy i = t + (fromIntegral $ i `div` cols) * ch

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