{-|
Module      : Monomer.Widgets.Containers.BoxShadow
Copyright   : (c) 2022 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

A rectangular drop-shadow. Normally used around alert boxes to give the illusion
they are floating above the widgets underneath them.
-}
{-# LANGUAGE Strict #-}

module Monomer.Widgets.Containers.BoxShadow (
  -- * Configuration
  BoxShadowCfg,
  -- * Constructors
  boxShadow,
  boxShadow_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~), (^.))
import Data.Default
import Data.Maybe

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

{-|
Configuration options for boxShadow:

- 'radius': the radius of the corners of the shadow.
- 'alignLeft': aligns the shadow to the left.
- 'alignCenter': aligns the shadow to the horizontal center.
- 'alignRight': aligns the shadow to the right.
- 'alignTop': aligns the shadow to the top.
- 'alignMiddle': aligns the shadow to the vertical middle.
- 'alignBottom': aligns the shadow to the bottom.
-}
data BoxShadowCfg = BoxShadowCfg {
  _bscRadius :: Maybe Double,
  _bscAlignH :: Maybe AlignH,
  _bscAlignV :: Maybe AlignV
}

instance Default BoxShadowCfg where
  def = BoxShadowCfg {
    _bscRadius = Nothing,
    _bscAlignH = Nothing,
    _bscAlignV = Nothing
  }

instance Semigroup BoxShadowCfg where
  (<>) c1 c2 = BoxShadowCfg {
    _bscRadius = _bscRadius c1 <|> _bscRadius c2,
    _bscAlignH = _bscAlignH c1 <|> _bscAlignH c2,
    _bscAlignV = _bscAlignV c1 <|> _bscAlignV c2
  }

instance Monoid BoxShadowCfg where
  mempty = def

instance CmbRadius BoxShadowCfg where
  radius r = def {
    _bscRadius = Just r
  }

instance CmbAlignLeft BoxShadowCfg where
  alignLeft_ False = def
  alignLeft_ True = def {
    _bscAlignH = Just ALeft
  }

instance CmbAlignCenter BoxShadowCfg where
  alignCenter_ False = def
  alignCenter_ True = def {
    _bscAlignH = Just ACenter
  }

instance CmbAlignRight BoxShadowCfg where
  alignRight_ False = def
  alignRight_ True = def {
    _bscAlignH = Just ARight
  }

instance CmbAlignTop BoxShadowCfg where
  alignTop_ False = def
  alignTop_ True = def {
    _bscAlignV = Just ATop
  }

instance CmbAlignMiddle BoxShadowCfg where
  alignMiddle_ False = def
  alignMiddle_ True = def {
    _bscAlignV = Just AMiddle
  }

instance CmbAlignBottom BoxShadowCfg where
  alignBottom_ False = def
  alignBottom_ True = def {
    _bscAlignV = Just ABottom
  }

-- | Creates a boxShadow around the provided content.
boxShadow
  :: WidgetNode s e  -- ^ The content to display inside the boxShadow.
  -> WidgetNode s e  -- ^ The created boxShadow.
boxShadow = boxShadow_ def

-- | Creates a boxShadow around the provided content. Accepts config.
boxShadow_
  :: [BoxShadowCfg]  -- ^ The config options for the boxShadow.
  -> WidgetNode s e  -- ^ The content to display inside the boxShadow.
  -> WidgetNode s e  -- ^ The created boxShadow.
boxShadow_ config child =
  defaultWidgetNode "boxShadow" (boxShadowWidget (mconcat config))
   & L.children .~ Seq.singleton child

boxShadowWidget :: BoxShadowCfg -> Widget s e
boxShadowWidget config = widget where
  widget = createContainer () def {
    containerGetSizeReq = getSizeReq,
    containerResize = resize,
    containerRender = render
  }

  shadowRadius = fromMaybe 8 (_bscRadius config)
  shadowDiameter = shadowRadius * 2

  getSizeReq wenv node children = (sizeReqW, sizeReqH) where
    sizeReqW = maybe (fixedSize 0) (addFixed shadowDiameter . _wniSizeReqW . _wnInfo) vchild
    sizeReqH = maybe (fixedSize 0) (addFixed shadowDiameter. _wniSizeReqH . _wnInfo) vchild
    vchildren = Seq.filter (_wniVisible . _wnInfo) children
    vchild = Seq.lookup 0 vchildren

  resize wenv node viewport children = (resultNode node, fmap assignArea children) where
    style = currentStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style viewport)
    
    assignArea child
      | visible = moveRect childOffset (subtractShadow contentArea)
      | otherwise = def
      where
        visible = (_wniVisible . _wnInfo) child
    
    childOffset = Point offsetX offsetY where
      theme = currentTheme wenv node
      shadowAlignH = fromMaybe (theme ^. L.shadowAlignH) (_bscAlignH config)
      shadowAlignV = fromMaybe (theme ^. L.shadowAlignV) (_bscAlignV config)
      offset = shadowRadius / 4
      offsetX = case shadowAlignH of
        ALeft -> offset
        ACenter -> 0
        ARight -> -offset
      offsetY = case shadowAlignV of
        ATop -> offset
        AMiddle -> 0
        ABottom -> -offset
  
  render wenv node renderer = do
    beginPath renderer
    setFillBoxGradient renderer (subtractShadow vp) shadowRadius shadowDiameter shadowColor transparent
    renderRect renderer vp
    fill renderer
    where
      style = currentStyle wenv node
      vp = getContentArea node style
      shadowColor = wenv ^. L.theme . L.basic . L.shadowColor
      transparent = rgba 0 0 0 0
  
  subtractShadow (Rect l t w h) = Rect l' t' w' h' where
    (l', w') = subtractDim l w
    (t', h') = subtractDim t h
    subtractDim pos size
      | size > shadowDiameter = (pos + shadowRadius, size - shadowDiameter)
      | otherwise = (pos + size / 2, 0)

addFixed :: Double -> SizeReq -> SizeReq
addFixed f sReq =
  sReq { _szrFixed = _szrFixed sReq + f }
