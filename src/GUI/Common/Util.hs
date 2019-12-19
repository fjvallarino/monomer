{-# LANGUAGE BangPatterns #-}

module GUI.Common.Util where

import Data.Default

import GUI.Common.Types

white      = RGB 255 255 255
black      = RGB   0   0   0
red        = RGB 255   0   0
green      = RGB   0 255   0
blue       = RGB   0   0 255
lightGray  = RGB 191 191 191
gray       = RGB 127 127 127
darkGray   = RGB  63  63  63

inRect :: Rect -> Point -> Bool
inRect (Rect x y w h) (Point x2 y2) = (x2 >= x && x2 < x + w) && (y2 >= y && y2 < y + h)

rotateList :: [a] -> [a]
rotateList [] = []
rotateList (x:xs) = xs ++ [x]

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just val) _ = Just val
firstJust _ value = value

justDef :: (Default a) => Maybe a -> a
justDef Nothing = def
justDef (Just val) = val

midPoint :: Point -> Point -> Point
midPoint (Point x1 y1) (Point x2 y2) = Point x3 y3 where
  x3 = (x2 + x1) / 2
  y3 = (y2 + y1) / 2

moveRect :: Rect -> Double -> Double -> Rect
moveRect (Rect x y w h) dx dy = Rect (x + dx) (y + dy) w h
