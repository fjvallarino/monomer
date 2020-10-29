module Monomer.Core.StyleUtil (
  key,
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
  styleFgColor,
  styleHlColor,
  getContentArea,
  addOuterSize,
  addOuterBounds,
  removeOuterSize,
  removeOuterBounds
) where

import Control.Lens ((&), (^.), (^?), (.~), (?~), _Just)
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
infixl 5 `visible`

key :: WidgetInstance s e -> Text -> WidgetInstance s e
key inst key = inst & L.key ?~ WidgetKey key

visible :: WidgetInstance s e -> Bool -> WidgetInstance s e
visible inst visibility = inst & L.visible .~ visibility

getContentArea :: StyleState -> WidgetInstance s e -> Rect
getContentArea style inst = removeOuterBounds style (_wiRenderArea inst)

instance Style_ Style where
  style oldStyle states = newStyle where
    state = mconcat states
    newStyle = oldStyle & L.basic ?~ state

instance Hover_ Style where
  hover oldStyle states = newStyle where
    state = mconcat states
    newStyle = oldStyle & L.hover ?~ state

instance Focus_ Style where
  focus oldStyle states = newStyle where
    state = mconcat states
    newStyle = oldStyle & L.focus ?~ state

instance Disabled_ Style where
  disabled oldStyle states = newStyle where
    state = mconcat states
    newStyle = oldStyle & L.disabled ?~ state

instance Style_ (WidgetInstance s e) where
  style inst states = inst & L.style .~ newStyle where
    state = mconcat states
    oldStyle = inst ^. L.style
    newStyle = oldStyle & L.basic ?~ state

instance Hover_ (WidgetInstance s e) where
  hover inst states = inst & L.style .~ newStyle where
    state = mconcat states
    oldStyle = inst ^. L.style
    newStyle = oldStyle & L.hover ?~ state

instance Focus_ (WidgetInstance s e) where
  focus inst states = inst & L.style .~ newStyle where
    state = mconcat states
    oldStyle = inst ^. L.style
    newStyle = oldStyle & L.focus ?~ state

instance Disabled_ (WidgetInstance s e) where
  disabled inst states = inst & L.style .~ newStyle where
    state = mconcat states
    oldStyle = inst ^. L.style
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

styleTextAlignH :: StyleState -> AlignH
styleTextAlignH style = fromMaybe def alignH where
  alignH = style ^? L.text . _Just . L.alignH . _Just

styleTextAlignV :: StyleState -> AlignV
styleTextAlignV style = fromMaybe def alignV where
  alignV = style ^? L.text . _Just . L.alignV . _Just

styleFgColor :: StyleState -> Color
styleFgColor style = fromMaybe def fgColor where
  fgColor = style ^? L.fgColor . _Just

styleHlColor :: StyleState -> Color
styleHlColor style = fromMaybe def hlColor where
  hlColor = style ^? L.hlColor . _Just

addOuterSize :: StyleState -> Size -> Size
addOuterSize style sz = final where
  border = addBorderSize sz (_sstBorder style)
  padding = addPaddingSize border (_sstPadding style)
  final = padding

removeOuterSize :: StyleState -> Size -> Size
removeOuterSize style sz = final where
  border = subtractBorderSize sz (_sstBorder style)
  padding = subtractPaddingSize border (_sstPadding style)
  final = padding

addOuterBounds :: StyleState -> Rect -> Rect
addOuterBounds style rect = final where
  border = addBorder rect (_sstBorder style)
  padding = addPadding border (_sstPadding style)
  final = padding

removeOuterBounds :: StyleState -> Rect -> Rect
removeOuterBounds style rect = final where
  border = subtractBorder rect (_sstBorder style)
  padding = subtractPadding border (_sstPadding style)
  final = padding

-- Internal
addBorderSize :: Size -> Maybe Border -> Size
addBorderSize sz border = nSize where
  (bl, br, bt, bb) = borderWidths border
  nSize = addToSize sz (bl + br) (bt + bb)

addPaddingSize :: Size -> Maybe Padding -> Size
addPaddingSize sz Nothing = sz
addPaddingSize sz (Just (Padding l r t b)) = nSize where
  nSize = addToSize sz (justDef l + justDef r) (justDef t + justDef b)

subtractBorderSize :: Size -> Maybe Border -> Size
subtractBorderSize sz border = nSize where
  (bl, br, bt, bb) = borderWidths border
  nSize = subtractFromSize sz (bl + br) (bt + bb)

subtractPaddingSize :: Size -> Maybe Padding -> Size
subtractPaddingSize sz Nothing = sz
subtractPaddingSize sz (Just (Padding l r t b)) = nSize where
  nSize = subtractFromSize sz (justDef l + justDef r) (justDef t + justDef b)

addBorder :: Rect -> Maybe Border -> Rect
addBorder rect border = nRect where
  (bl, br, bt, bb) = borderWidths border
  nRect = addToRect rect bl br bt bb

addPadding :: Rect -> Maybe Padding -> Rect
addPadding rect Nothing = rect
addPadding rect (Just (Padding l r t b)) = nRect where
  nRect = addToRect rect (justDef l) (justDef r) (justDef t) (justDef b)

subtractBorder :: Rect -> Maybe Border -> Rect
subtractBorder rect border = nRect where
  (bl, br, bt, bb) = borderWidths border
  nRect = subtractFromRect rect bl br bt bb

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
