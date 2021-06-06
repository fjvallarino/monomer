{-|
Module      : Monomer.Core.StyleUtil
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for style types.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Core.StyleUtil (
  style,
  hover,
  focus,
  disabled,
  styleFont,
  styleFontSize,
  styleFontColor,
  styleTextAlignH,
  styleTextAlignV,
  styleBgColor,
  styleFgColor,
  styleSndColor,
  styleHlColor,
  getContentArea,
  getOuterSize,
  addOuterSize,
  addOuterBounds,
  removeOuterSize,
  removeOuterBounds
) where

import Control.Lens ((&), (^.), (^?), (.~), (?~), _Just, non)
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer.Common
import Monomer.Core.Combinators
import Monomer.Core.StyleTypes
import Monomer.Core.WidgetTypes
import Monomer.Graphics.Types

import qualified Monomer.Lens as L

-- | Returns the content area (i.e., ignoring border and padding) of the node.
getContentArea :: StyleState -> WidgetNode s e -> Rect
getContentArea style node = fromMaybe def area where
  area = removeOuterBounds style (node ^. L.info . L.viewport)

instance CmbStyle Style where
  style oldStyle states = newStyle where
    state = mconcat states
    newStyle = oldStyle & L.basic ?~ state

instance CmbHover Style where
  hover oldStyle states = newStyle where
    state = mconcat states
    newStyle = oldStyle & L.hover ?~ state

instance CmbFocus Style where
  focus oldStyle states = newStyle where
    state = mconcat states
    newStyle = oldStyle & L.focus ?~ state

instance CmbFocusHover Style where
  focusHover oldStyle states = newStyle where
    state = mconcat states
    newStyle = oldStyle & L.focusHover ?~ state

instance CmbActive Style where
  active oldStyle states = newStyle where
    state = mconcat states
    newStyle = oldStyle & L.active ?~ state

instance CmbDisabled Style where
  disabled oldStyle states = newStyle where
    state = mconcat states
    newStyle = oldStyle & L.disabled ?~ state

instance CmbKey (WidgetNode s e) Text where
  key node key = node & L.info . L.key ?~ WidgetKey key

instance CmbEnabled (WidgetNode s e) where
  enabled node state = node & L.info . L.enabled .~ state

instance CmbVisible (WidgetNode s e) where
  visible node visibility = node & L.info . L.visible .~ visibility

instance CmbStyle (WidgetNode s e) where
  style node states = node & L.info . L.style .~ newStyle where
    state = mconcat states
    oldStyle = node ^. L.info . L.style
    newStyle = oldStyle & L.basic ?~ state

instance CmbHover (WidgetNode s e) where
  hover node states = node & L.info . L.style .~ newStyle where
    state = mconcat states
    oldStyle = node ^. L.info . L.style
    newStyle = oldStyle & L.hover ?~ state

instance CmbFocus (WidgetNode s e) where
  focus node states = node & L.info . L.style .~ newStyle where
    state = mconcat states
    oldStyle = node ^. L.info . L.style
    newStyle = oldStyle & L.focus ?~ state

instance CmbFocusHover (WidgetNode s e) where
  focusHover node states = node & L.info . L.style .~ newStyle where
    state = mconcat states
    oldStyle = node ^. L.info . L.style
    newStyle = oldStyle & L.focusHover ?~ state

instance CmbActive (WidgetNode s e) where
  active node states = node & L.info . L.style .~ newStyle where
    state = mconcat states
    oldStyle = node ^. L.info . L.style
    newStyle = oldStyle & L.active ?~ state

instance CmbDisabled (WidgetNode s e) where
  disabled node states = node & L.info . L.style .~ newStyle where
    state = mconcat states
    oldStyle = node ^. L.info . L.style
    newStyle = oldStyle & L.disabled ?~ state

-- | Returns the font of the given style state, or the default.
styleFont :: StyleState -> Font
styleFont style = fromMaybe def font where
  font = style ^? L.text . _Just  . L.font . _Just

-- | Returns the font size of the given style state, or the default.
styleFontSize :: StyleState -> FontSize
styleFontSize style = fromMaybe def fontSize where
  fontSize = style ^? L.text . _Just . L.fontSize . _Just

-- | Returns the font color of the given style state, or the default.
styleFontColor :: StyleState -> Color
styleFontColor style = fromMaybe def fontColor where
  fontColor = style ^? L.text . _Just . L.fontColor . _Just

-- | Returns the horizontal alignment of the given style state, or the default.
styleTextAlignH :: StyleState -> AlignTH
styleTextAlignH style = fromMaybe def alignH where
  alignH = style ^? L.text . _Just . L.alignH . _Just

-- | Returns the vertical alignment of the given style state, or the default.
styleTextAlignV :: StyleState -> AlignTV
styleTextAlignV style = fromMaybe def alignV where
  alignV = style ^? L.text . _Just . L.alignV . _Just

-- | Returns the background color of the given style state, or the default.
styleBgColor :: StyleState -> Color
styleBgColor style = fromMaybe def bgColor where
  bgColor = style ^? L.bgColor . _Just

-- | Returns the foreground color of the given style state, or the default.
styleFgColor :: StyleState -> Color
styleFgColor style = fromMaybe def fgColor where
  fgColor = style ^? L.fgColor . _Just

-- | Returns the secondary color of the given style state, or the default.
styleSndColor :: StyleState -> Color
styleSndColor style = fromMaybe def sndColor where
  sndColor = style ^? L.sndColor . _Just

-- | Returns the highlight color of the given style state, or the default.
styleHlColor :: StyleState -> Color
styleHlColor style = fromMaybe def hlColor where
  hlColor = style ^? L.hlColor . _Just

-- | Returns the size used by border and padding.
getOuterSize :: StyleState -> Size
getOuterSize style = fromMaybe def size where
  size = addOuterSize style def

-- | Adds border and padding to the given size.
addOuterSize :: StyleState -> Size -> Maybe Size
addOuterSize style sz =
  addBorderSize sz (_sstBorder style)
    >>= (`addPaddingSize` _sstPadding style)

-- | Removes border and padding from the given size.
removeOuterSize :: StyleState -> Size -> Maybe Size
removeOuterSize style sz =
  subtractBorderSize sz (_sstBorder style)
    >>= (`subtractPaddingSize` _sstPadding style)

-- | Adds border and padding to the given rect.
addOuterBounds :: StyleState -> Rect -> Maybe Rect
addOuterBounds style rect =
  addBorder rect (_sstBorder style)
    >>= (`addPadding` _sstPadding style)

-- | Removes border and padding from the given rect.
removeOuterBounds :: StyleState -> Rect -> Maybe Rect
removeOuterBounds style rect =
  subtractBorder rect (_sstBorder style)
    >>= (`subtractPadding` _sstPadding style)

-- Internal
addBorderSize :: Size -> Maybe Border -> Maybe Size
addBorderSize sz border = nSize where
  (bl, br, bt, bb) = borderWidths border
  nSize = addToSize sz (bl + br) (bt + bb)

addPaddingSize :: Size -> Maybe Padding -> Maybe Size
addPaddingSize sz Nothing = Just sz
addPaddingSize sz (Just (Padding l r t b)) = nSize where
  nSize = addToSize sz (justDef l + justDef r) (justDef t + justDef b)

subtractBorderSize :: Size -> Maybe Border -> Maybe Size
subtractBorderSize sz border = nSize where
  (bl, br, bt, bb) = borderWidths border
  nSize = subtractFromSize sz (bl + br) (bt + bb)

subtractPaddingSize :: Size -> Maybe Padding -> Maybe Size
subtractPaddingSize sz Nothing = Just sz
subtractPaddingSize sz (Just (Padding l r t b)) = nSize where
  nSize = subtractFromSize sz (justDef l + justDef r) (justDef t + justDef b)

addBorder :: Rect -> Maybe Border -> Maybe Rect
addBorder rect border = nRect where
  (bl, br, bt, bb) = borderWidths border
  nRect = addToRect rect bl br bt bb

addPadding :: Rect -> Maybe Padding -> Maybe Rect
addPadding rect Nothing = Just rect
addPadding rect (Just (Padding l r t b)) = nRect where
  nRect = addToRect rect (justDef l) (justDef r) (justDef t) (justDef b)

subtractBorder :: Rect -> Maybe Border -> Maybe Rect
subtractBorder rect border = nRect where
  (bl, br, bt, bb) = borderWidths border
  nRect = subtractFromRect rect bl br bt bb

subtractPadding :: Rect -> Maybe Padding -> Maybe Rect
subtractPadding rect Nothing = Just rect
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
