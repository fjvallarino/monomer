module Monomer.Common.StyleUtil (
  mergeThemeStyle,
  addOuterSize,
  addOuterBounds,
  removeOuterSize,
  removeOuterBounds,
  subtractMargin
) where

import Data.Default
import Data.Maybe

import Monomer.Common.Geometry
import Monomer.Common.Style

mergeThemeStyle :: ThemeState -> StyleState -> StyleState
mergeThemeStyle theme style = newStyle where
  themeText = Just def {
    _txsFont = Just $ _thsFont theme,
    _txsFontSize = Just $ _thsFontSize theme,
    _txsColor = Just $ _thsColor theme
  }
  textFgColor = Just $ _thsFgColor theme
  newStyle = style {
    _sstFgColor = _sstFgColor style <> textFgColor,
    _sstText = _sstText style <> themeText
  }

addOuterSize :: StyleState -> Size -> Size
addOuterSize style sz = final where
  margin = addMarginSize sz (_sstMargin style)
  border = addBorderSize margin (_sstBorder style)
  padding = addPaddingSize border (_sstPadding style)
  final = padding

removeOuterSize :: StyleState -> Size -> Size
removeOuterSize style sz = final where
  margin = subtractMarginSize sz (_sstMargin style)
  border = subtractBorderSize margin (_sstBorder style)
  padding = subtractPaddingSize border (_sstPadding style)
  final = padding

addOuterBounds :: StyleState -> Rect -> Rect
addOuterBounds style viewport = final where
  margin = addMargin viewport (_sstMargin style)
  border = addBorder margin (_sstBorder style)
  padding = addPadding border (_sstPadding style)
  final = padding

removeOuterBounds :: StyleState -> Rect -> Rect
removeOuterBounds style viewport = final where
  margin = subtractMargin viewport (_sstMargin style)
  border = subtractBorder margin (_sstBorder style)
  padding = subtractPadding border (_sstPadding style)
  final = padding

-- Internal
addBorderSize :: Size -> Maybe Border -> Size
addBorderSize sz border = nSize where
  (bl, br, bt, bb) = borderWidths border
  nSize = addToSize sz (bl + br) (bt + bb)

addMarginSize :: Size -> Maybe Margin -> Size
addMarginSize sz Nothing = sz
addMarginSize sz (Just (Margin l r t b)) = nSize where
  nSize = addToSize sz (justDef l + justDef r) (justDef t + justDef b)

addPaddingSize :: Size -> Maybe Padding -> Size
addPaddingSize sz Nothing = sz
addPaddingSize sz (Just (Padding l r t b)) = nSize where
  nSize = addToSize sz (justDef l + justDef r) (justDef t + justDef b)

subtractBorderSize :: Size -> Maybe Border -> Size
subtractBorderSize sz border = nSize where
  (bl, br, bt, bb) = borderWidths border
  nSize = subtractFromSize sz (bl + br) (bt + bb)

subtractMarginSize :: Size -> Maybe Margin -> Size
subtractMarginSize sz Nothing = sz
subtractMarginSize sz (Just (Margin l r t b)) = nSize where
  nSize = subtractFromSize sz (justDef l + justDef r) (justDef t + justDef b)

subtractPaddingSize :: Size -> Maybe Padding -> Size
subtractPaddingSize sz Nothing = sz
subtractPaddingSize sz (Just (Padding l r t b)) = nSize where
  nSize = subtractFromSize sz (justDef l + justDef r) (justDef t + justDef b)

addBorder :: Rect -> Maybe Border -> Rect
addBorder rect border = nRect where
  (bl, br, bt, bb) = borderWidths border
  nRect = addToRect rect bl br bt bb

addMargin :: Rect -> Maybe Margin -> Rect
addMargin rect Nothing = rect
addMargin rect (Just (Margin l r t b)) = nRect where
  nRect = addToRect rect (justDef l) (justDef r) (justDef t) (justDef b)

addPadding :: Rect -> Maybe Padding -> Rect
addPadding rect Nothing = rect
addPadding rect (Just (Padding l r t b)) = nRect where
  nRect = addToRect rect (justDef l) (justDef r) (justDef t) (justDef b)

subtractBorder :: Rect -> Maybe Border -> Rect
subtractBorder rect border = nRect where
  (bl, br, bt, bb) = borderWidths border
  nRect = subtractFromRect rect bl br bt bb

subtractMargin :: Rect -> Maybe Margin -> Rect
subtractMargin rect Nothing = rect
subtractMargin rect (Just (Margin l r t b)) = nRect where
  nRect = subtractFromRect rect (justDef l) (justDef r) (justDef t) (justDef b)

subtractPadding :: Rect -> Maybe Padding -> Rect
subtractPadding rect Nothing = rect
subtractPadding rect (Just (Padding l r t b)) = nRect where
  nRect = subtractFromRect rect (justDef l) (justDef r) (justDef t) (justDef b)

borderWidths :: Maybe Border -> (Double, Double, Double, Double)
borderWidths Nothing = (0, 0, 0, 0)
borderWidths (Just border) = (bl, br, bt, bb) where
  bl = maybe 0 _bsWidth (_brdLeft border)
  br = maybe 0 _bsWidth (_brdRight border)
  bt = maybe 0 _bsWidth (_brdTop border)
  bb = maybe 0 _bsWidth (_brdBottom border)

justDef :: (Default a) => Maybe a -> a
justDef val = fromMaybe def val
