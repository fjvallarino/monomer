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
-}
newtype BoxShadowCfg = BoxShadowCfg {
  _bscRadius :: Maybe Double
}

instance Default BoxShadowCfg where
  def = BoxShadowCfg {
    _bscRadius = Nothing
  }

instance Semigroup BoxShadowCfg where
  (<>) c1 c2 = BoxShadowCfg {
    _bscRadius = _bscRadius c1 <|> _bscRadius c2
  }

instance Monoid BoxShadowCfg where
  mempty = def

instance CmbRadius BoxShadowCfg where
  radius r = BoxShadowCfg {
    _bscRadius = Just r
  }

-- | Creates a boxShadow around the provided content.
boxShadow
  :: WidgetNode s e  -- ^ The content to display inside the boxShadow.
  -> WidgetNode s e  -- ^ The created boxShadow.
boxShadow = boxShadow_ def

-- | Creates a boxShadow around the provided content. Accepts config.
boxShadow_
  :: BoxShadowCfg    -- ^ The config options for the boxShadow.
  -> WidgetNode s e  -- ^ The content to display inside the boxShadow.
  -> WidgetNode s e  -- ^ The created boxShadow.
boxShadow_ config child =
  defaultWidgetNode "boxShadow" (boxShadowWidget config)
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
    offset = -shadowRadius / 4 -- so the light appears to be coming from the top center, like in MacOS
    assignArea child
      | visible = moveRect (Point 0 offset) (subtractShadow contentArea)
      | otherwise = def
      where
        visible = (_wniVisible . _wnInfo) child
  
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
