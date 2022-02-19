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
{-# LANGUAGE Strict #-}

module Monomer.Core.StyleUtil (
  getContentArea,
  nodeKey,
  nodeEnabled,
  nodeVisible,
  nodeFocusable,
  styleFont,
  styleFontSize,
  styleFontSpaceH,
  styleFontSpaceV,
  styleFontColor,
  styleTextAlignH,
  styleTextAlignV,
  styleTextLineBreak,
  styleBgColor,
  styleFgColor,
  styleSndColor,
  styleHlColor,
  getOuterSize,
  addOuterSize,
  addOuterBounds,
  removeOuterSize,
  removeOuterBounds,
  addBorder,
  addPadding,
  subtractBorder,
  subtractPadding,
  subtractBorderFromRadius,
  mapStyleStates
) where

import Control.Lens ((&), (^.), (^?), (.~), (+~), (%~), (?~), _Just, non)
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer.Common
import Monomer.Core.Combinators
import Monomer.Core.StyleTypes
import Monomer.Core.WidgetTypes
import Monomer.Graphics.Types
import Monomer.Helper

import qualified Monomer.Core.Lens as L

instance CmbStyleBasic Style where
  styleBasic oldStyle states = newStyle where
    newStyle = oldStyle & L.basic .~ maybeConcat states

instance CmbStyleHover Style where
  styleHover oldStyle states = newStyle where
    newStyle = oldStyle & L.hover .~ maybeConcat states

instance CmbStyleFocus Style where
  styleFocus oldStyle states = newStyle where
    newStyle = oldStyle & L.focus .~ maybeConcat states

instance CmbStyleFocusHover Style where
  styleFocusHover oldStyle states = newStyle where
    newStyle = oldStyle & L.focusHover .~ maybeConcat states

instance CmbStyleActive Style where
  styleActive oldStyle states = newStyle where
    newStyle = oldStyle & L.active .~ maybeConcat states

instance CmbStyleDisabled Style where
  styleDisabled oldStyle states = newStyle where
    newStyle = oldStyle & L.disabled .~ maybeConcat states

instance CmbStyleBasic (WidgetNode s e) where
  styleBasic node states = node & L.info . L.style .~ newStyle where
    state = mconcat states
    oldStyle = node ^. L.info . L.style
    newStyle = oldStyle & L.basic ?~ state

instance CmbStyleHover (WidgetNode s e) where
  styleHover node states = node & L.info . L.style .~ newStyle where
    state = mconcat states
    oldStyle = node ^. L.info . L.style
    newStyle = oldStyle & L.hover ?~ state

instance CmbStyleFocus (WidgetNode s e) where
  styleFocus node states = node & L.info . L.style .~ newStyle where
    state = mconcat states
    oldStyle = node ^. L.info . L.style
    newStyle = oldStyle & L.focus ?~ state

instance CmbStyleFocusHover (WidgetNode s e) where
  styleFocusHover node states = node & L.info . L.style .~ newStyle where
    state = mconcat states
    oldStyle = node ^. L.info . L.style
    newStyle = oldStyle & L.focusHover ?~ state

instance CmbStyleActive (WidgetNode s e) where
  styleActive node states = node & L.info . L.style .~ newStyle where
    state = mconcat states
    oldStyle = node ^. L.info . L.style
    newStyle = oldStyle & L.active ?~ state

instance CmbStyleDisabled (WidgetNode s e) where
  styleDisabled node states = node & L.info . L.style .~ newStyle where
    state = mconcat states
    oldStyle = node ^. L.info . L.style
    newStyle = oldStyle & L.disabled ?~ state

infixl 5 `nodeKey`
infixl 5 `nodeEnabled`
infixl 5 `nodeVisible`
infixl 5 `nodeFocusable`

-- | Sets the key of the given node.
nodeKey :: WidgetNode s e -> Text -> WidgetNode s e
nodeKey node key = node & L.info . L.key ?~ WidgetKey key

-- | Sets whether the given node is enabled.
nodeEnabled :: WidgetNode s e -> Bool -> WidgetNode s e
nodeEnabled node state = node & L.info . L.enabled .~ state

-- | Sets whether the given node is visible.
nodeVisible :: WidgetNode s e -> Bool -> WidgetNode s e
nodeVisible node visibility = node & L.info . L.visible .~ visibility

-- | Sets whether the given node is focusable.
nodeFocusable :: WidgetNode s e -> Bool -> WidgetNode s e
nodeFocusable node isFocusable = node & L.info . L.focusable .~ isFocusable

-- | Returns the content area (i.e., ignoring border and padding) of the node.
getContentArea :: WidgetNode s e -> StyleState -> Rect
getContentArea node style = fromMaybe def area where
  area = removeOuterBounds style (node ^. L.info . L.viewport)

-- | Returns the font of the given style state, or the default.
styleFont :: StyleState -> Font
styleFont style = fromMaybe def font where
  font = style ^? L.text . _Just  . L.font . _Just

-- | Returns the font size of the given style state, or the default.
styleFontSize :: StyleState -> FontSize
styleFontSize style = fromMaybe def fontSize where
  fontSize = style ^? L.text . _Just . L.fontSize . _Just

-- | Returns the horizontal spacing of the given style state, or the default.
styleFontSpaceH :: StyleState -> FontSpace
styleFontSpaceH style = fromMaybe def fontSpaceH where
  fontSpaceH = style ^? L.text . _Just . L.fontSpaceH . _Just

-- | Returns the vertical spacing of the given style state, or the default.
styleFontSpaceV :: StyleState -> FontSpace
styleFontSpaceV style = fromMaybe def fontSpaceV where
  fontSpaceV = style ^? L.text . _Just . L.fontSpaceV . _Just

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

-- | Returns the line break option of the given style state, or the
styleTextLineBreak :: StyleState -> LineBreak
styleTextLineBreak style = fromMaybe def lineBreak where
  lineBreak = style ^? L.text . _Just . L.lineBreak . _Just

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

-- | Adds border widths to the given rect.
addBorder :: Rect -> Maybe Border -> Maybe Rect
addBorder rect border = nRect where
  (bl, br, bt, bb) = borderWidths border
  nRect = addToRect rect bl br bt bb

-- | Adds padding the given rect.
addPadding :: Rect -> Maybe Padding -> Maybe Rect
addPadding rect Nothing = Just rect
addPadding rect (Just (Padding l r t b)) = nRect where
  nRect = addToRect rect (justDef l) (justDef r) (justDef t) (justDef b)

-- | Subtracts border widths from the given rect.
subtractBorder :: Rect -> Maybe Border -> Maybe Rect
subtractBorder rect border = nRect where
  (bl, br, bt, bb) = borderWidths border
  nRect = subtractFromRect rect bl br bt bb

-- | Subbtracts padding from the given rect.
subtractPadding :: Rect -> Maybe Padding -> Maybe Rect
subtractPadding rect Nothing = Just rect
subtractPadding rect (Just (Padding l r t b)) = nRect where
  nRect = subtractFromRect rect (justDef l) (justDef r) (justDef t) (justDef b)

{-|
Subtracts border width from radius. This is useful when rendering nested shapes
with rounded corners, which would otherwise have gaps in the corners.
-}
subtractBorderFromRadius :: Maybe Border -> Radius -> Radius
subtractBorderFromRadius border (Radius rtl rtr rbl rbr) = newRadius where
  (bl, br, bt, bb) = borderWidths border
  ntl = rtl & _Just . L.width %~ \w -> max 0 (w - max bl bt)
  ntr = rtr & _Just . L.width %~ \w -> max 0 (w - max br bt)
  nbl = rbl & _Just . L.width %~ \w -> max 0 (w - max bl bb)
  nbr = rbr & _Just . L.width %~ \w -> max 0 (w - max br bb)
  newRadius = Radius ntl ntr nbl nbr

{-|
Applies a function to all states of a given style. Useful when trying to set or
reset the same property in all different states.
-}
mapStyleStates :: (StyleState -> StyleState) -> Style -> Style
mapStyleStates fn style = newStyle where
  newStyle = Style {
    _styleBasic = fn <$> _styleBasic style,
    _styleHover = fn <$> _styleHover style,
    _styleFocus = fn <$> _styleFocus style,
    _styleFocusHover = fn <$> _styleFocusHover style,
    _styleActive = fn <$> _styleActive style,
    _styleDisabled = fn <$> _styleDisabled style
  }

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

borderWidths :: Maybe Border -> (Double, Double, Double, Double)
borderWidths Nothing = (0, 0, 0, 0)
borderWidths (Just border) = (bl, br, bt, bb) where
  bl = maybe 0 _bsWidth (_brdLeft border)
  br = maybe 0 _bsWidth (_brdRight border)
  bt = maybe 0 _bsWidth (_brdTop border)
  bb = maybe 0 _bsWidth (_brdBottom border)

justDef :: (Default a) => Maybe a -> a
justDef val = fromMaybe def val
