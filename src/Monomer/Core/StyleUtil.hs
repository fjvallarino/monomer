{-# LANGUAGE FlexibleContexts #-}

module Monomer.Core.StyleUtil (
  key,
  globalKey,
  style,
  hover,
  focus,
  visible,
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
  removeOuterBounds,
  setStyleValue
) where

import Control.Lens ((&), (^.), (^?), (.~), (?~), _Just, non)
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer.Core.BasicTypes
import Monomer.Core.Combinators
import Monomer.Core.StyleTypes
import Monomer.Core.WidgetTypes
import Monomer.Graphics.Types

import qualified Monomer.Lens as L

infixl 5 `key`
infixl 5 `globalKey`
infixl 5 `visible`

key :: WidgetNode s e -> Text -> WidgetNode s e
key node key = node & L.info . L.key ?~ WidgetKeyLocal key

globalKey :: WidgetNode s e -> Text -> WidgetNode s e
globalKey node key = node & L.info . L.key ?~ WidgetKeyGlobal key

visible :: WidgetNode s e -> Bool -> WidgetNode s e
visible node visibility = node & L.info . L.visible .~ visibility

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

instance CmbEnabled (WidgetNode s e) where
  enabled node state = node & L.info . L.enabled .~ state

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

styleFont :: StyleState -> Font
styleFont style = fromMaybe def font where
  font = style ^? L.text . _Just  . L.font . _Just

styleFontSize :: StyleState -> FontSize
styleFontSize style = fromMaybe def fontSize where
  fontSize = style ^? L.text . _Just . L.fontSize . _Just

styleFontColor :: StyleState -> Color
styleFontColor style = fromMaybe def fontColor where
  fontColor = style ^? L.text . _Just . L.fontColor . _Just

styleTextAlignH :: StyleState -> AlignTH
styleTextAlignH style = fromMaybe def alignH where
  alignH = style ^? L.text . _Just . L.alignH . _Just

styleTextAlignV :: StyleState -> AlignTV
styleTextAlignV style = fromMaybe def alignV where
  alignV = style ^? L.text . _Just . L.alignV . _Just

styleBgColor :: StyleState -> Color
styleBgColor style = fromMaybe def bgColor where
  bgColor = style ^? L.bgColor . _Just

styleFgColor :: StyleState -> Color
styleFgColor style = fromMaybe def fgColor where
  fgColor = style ^? L.fgColor . _Just

styleSndColor :: StyleState -> Color
styleSndColor style = fromMaybe def sndColor where
  sndColor = style ^? L.sndColor . _Just

styleHlColor :: StyleState -> Color
styleHlColor style = fromMaybe def hlColor where
  hlColor = style ^? L.hlColor . _Just

getOuterSize :: StyleState -> Size
getOuterSize style = fromMaybe def size where
  size = addOuterSize style def

addOuterSize :: StyleState -> Size -> Maybe Size
addOuterSize style sz =
  addBorderSize sz (_sstBorder style)
    >>= (`addPaddingSize` _sstPadding style)

removeOuterSize :: StyleState -> Size -> Maybe Size
removeOuterSize style sz =
  subtractBorderSize sz (_sstBorder style)
    >>= (`subtractPaddingSize` _sstPadding style)

addOuterBounds :: StyleState -> Rect -> Maybe Rect
addOuterBounds style rect =
  addBorder rect (_sstBorder style)
    >>= (`addPadding` _sstPadding style)

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

setStyleValue style fieldL op value = newStyle where
  val = _styleActive style
  newStyle = style
    & op (L.basic . non def . fieldL) value
    & op (L.hover . non def . fieldL) value
    & op (L.focus . non def . fieldL) value
    & op (L.focusHover . non def . fieldL) value
    & op (L.active . non def . fieldL) value
    & op (L.disabled . non def . fieldL) value
