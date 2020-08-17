module Monomer.Event.Mouse where

import qualified SDL

import Monomer.Common.Geometry
import Monomer.Event.Types

mouseClick :: Point -> SDL.EventPayload -> Maybe SystemEvent
mouseClick mousePos (SDL.MouseButtonEvent eventData) = systemEvent where
    button = case SDL.mouseButtonEventButton eventData of
      SDL.ButtonLeft -> Just LeftBtn
      SDL.ButtonRight -> Just RightBtn
      _ -> Nothing
    action = case SDL.mouseButtonEventMotion eventData of
      SDL.Pressed -> PressedBtn
      SDL.Released -> ReleasedBtn
    systemEvent = fmap (\btn -> ButtonAction mousePos btn action) button
mouseClick _ _ = Nothing

mouseMoveEvent :: Double -> Point -> SDL.EventPayload -> Maybe SystemEvent
mouseMoveEvent dpr mousePos (SDL.MouseMotionEvent _) = Just $ Move mousePos
mouseMoveEvent dpr mousePos _ = Nothing

mouseMoveLeave :: Double -> Point -> SDL.EventPayload -> Maybe SystemEvent
mouseMoveLeave dpr mousePos SDL.WindowLostMouseFocusEvent{} = evt where
  evt = Just $ Move (Point (-1) (-1))
mouseMoveLeave dpr mousePos _ = Nothing

mouseWheelEvent :: Double -> Point -> SDL.EventPayload -> Maybe SystemEvent
mouseWheelEvent dpr mousePos (SDL.MouseWheelEvent eventData) = systemEvent where
  wheelDirection = case SDL.mouseWheelEventDirection eventData of
    SDL.ScrollNormal -> WheelNormal
    SDL.ScrollFlipped -> WheelFlipped
  SDL.V2 x y = SDL.mouseWheelEventPos eventData
  wheelDelta = Point (fromIntegral x * dpr) (fromIntegral y * dpr)
  systemEvent = case SDL.mouseWheelEventWhich eventData of
    SDL.Mouse _ -> Just $ WheelScroll mousePos wheelDelta wheelDirection
    SDL.Touch -> Nothing
mouseWheelEvent dpr mousePos _ = Nothing
