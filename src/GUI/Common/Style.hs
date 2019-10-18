{-# LANGUAGE TemplateHaskell #-}

module GUI.Common.Style where

import Data.Default
import Lens.Micro
import Lens.Micro.TH (makeLenses)

import GUI.Common.Core

data FontInstance = FontInstance

data Theme = Theme {
  _backgroundColor :: Color,
  _primaryColor :: Color,
  _secondaryColor :: Color,
  _palette :: [Color],
  _titleFont :: FontInstance,
  _subtitleFont :: FontInstance,
  _labelFont :: FontInstance,
  _messageFont :: FontInstance
}


-- | Basic styling attributes
--
-- Remember adjacent margin collapse behavior
data Style =
  Style {
    _padding :: Maybe Padding,
    _bgRadius :: Maybe Radius,
    _bgColor :: Maybe Color,
    _border :: Maybe Border,
    _textStyle :: Maybe TextStyle
  } deriving (Show, Eq)

instance Monoid Style where
  mempty = Style {
    _padding = Nothing,
    _bgRadius = Nothing,
    _bgColor = Nothing,
    _border = Nothing,
    _textStyle = Nothing
  }

instance Semigroup Style where
  (<>) style1 style2 = Style {
    _padding = (_padding style2) <> (_padding style1),
    _bgRadius = (_bgRadius style2) <> (_bgRadius style1),
    _bgColor = firstJust (_bgColor style2) (_bgColor style1),
    _border = (_border style2) <> (_border style1),
    _textStyle = (_textStyle style2) <> (_textStyle style1)
  }

data Padding = Padding {
  _pLeft   :: Maybe Double,
  _pRight  :: Maybe Double,
  _pTop    :: Maybe Double,
  _pBottom :: Maybe Double
} deriving (Show, Eq)

instance Semigroup Padding where
  (<>) p1 p2 = Padding {
    _pLeft = firstJust (_pLeft p2) (_pLeft p1),
    _pRight = firstJust (_pRight p2) (_pRight p1),
    _pTop = firstJust (_pTop p2) (_pTop p1),
    _pBottom = firstJust (_pBottom p2) (_pBottom p1)
  }

instance Monoid Padding where
  mempty = Padding {
    _pLeft = Nothing,
    _pRight = Nothing,
    _pTop = Nothing,
    _pBottom = Nothing
  }

data BorderSide = BorderSide {
  _bsWidth :: Double,
  _bsColor :: Color
} deriving (Show, Eq)

instance Semigroup BorderSide where
  (<>) _ b2 = b2

instance Default BorderSide where
  def = BorderSide {
    _bsWidth = 0,
    _bsColor = def
  }

data Border = Border {
  _bLeft   :: Maybe BorderSide,
  _bRight  :: Maybe BorderSide,
  _bTop    :: Maybe BorderSide,
  _bBottom :: Maybe BorderSide,
  _bRadius :: Maybe Radius
} deriving (Show, Eq)

instance Semigroup Border where
  (<>) b1 b2 = Border {
    _bLeft = (_bLeft b2) <> (_bLeft b1),
    _bRight = (_bRight b2) <> (_bRight b1),
    _bTop = (_bTop b2) <> (_bTop b1),
    _bBottom = (_bBottom b2) <> (_bBottom b1),
    _bRadius = (_bRadius b2) <> (_bRadius b1)
  }

instance Monoid Border where
  mempty = Border {
    _bLeft = def,
    _bRight = def,
    _bTop = def,
    _bBottom = def,
    _bRadius = def
  }

data Radius = Radius {
  _rTopLeft :: Maybe Double,
  _rTopRight :: Maybe Double,
  _rBottomLeft :: Maybe Double,
  _rBottomRight :: Maybe Double
} deriving (Show, Eq)

instance Semigroup Radius where
  (<>) _ br2 = br2

instance Default Radius where
  def = Radius {
    _rTopLeft = Nothing,
    _rTopRight = Nothing,
    _rBottomLeft = Nothing,
    _rBottomRight = Nothing
  }

data TextStyle = TextStyle {
  _tsFont :: Maybe String,
  _tsFontSize :: Maybe Double,
  _tsColor :: Maybe Color,
  _tsAlignH :: Maybe AlignH,
  _tsAlignV :: Maybe AlignV
} deriving (Show, Eq)

instance Semigroup TextStyle where
  (<>) ts1 ts2 = TextStyle {
    _tsFont = firstJust (_tsFont ts2) (_tsFont ts1),
    _tsFontSize = firstJust (_tsFontSize ts2) (_tsFontSize ts1),
    _tsColor = firstJust (_tsColor ts2) (_tsColor ts1),
    _tsAlignH = firstJust (_tsAlignH ts2) (_tsAlignH ts1),
    _tsAlignV = firstJust (_tsAlignV ts2) (_tsAlignV ts1)
  }

instance Monoid TextStyle where
  mempty = TextStyle {
    _tsFont = Nothing,
    _tsFontSize = Nothing,
    _tsColor = Nothing,
    _tsAlignH = Nothing,
    _tsAlignV = Nothing
  }

border :: Double -> Color -> Double -> Style
border width color radius = mempty {
  _border = Just mempty {
    _bLeft = Just (BorderSide width color),
    _bRight = Just (BorderSide width color),
    _bTop = Just (BorderSide width color),
    _bBottom = Just (BorderSide width color),
    _bRadius = Just (Radius (Just radius) (Just radius) (Just radius) (Just radius))
  }
}

borderTop :: Double -> Color -> Style
borderTop width color = mempty {
  _border = Just mempty {
    _bTop = Just (BorderSide width color)
  }
}

borderBottom :: Double -> Color -> Style
borderBottom width color = mempty {
  _border = Just mempty {
    _bBottom = Just (BorderSide width color)
  }
}

borderLeft :: Double -> Color -> Style
borderLeft width color = mempty {
  _border = Just mempty {
    _bLeft = Just (BorderSide width color)
  }
}

borderRight :: Double -> Color -> Style
borderRight width color = mempty {
  _border = Just mempty {
    _bRight = Just (BorderSide width color)
  }
}

bgColor :: Color -> Style
bgColor color = mempty { _bgColor = (Just color) }

bgRadius :: Double -> Style
bgRadius rad = mempty { _bgRadius = (Just (Radius jrad jrad jrad jrad)) } where
  jrad = Just rad

textColor :: Color -> Style
textColor color = mempty {
  _textStyle = Just $ mempty {
    _tsColor = Just color
  }
}

textSize :: Double -> Style
textSize size = mempty {
  _textStyle = Just $ mempty {
    _tsFontSize = Just size
  }
}

textAlignH :: AlignH -> Style
textAlignH alignH = mempty {
  _textStyle = Just $ mempty {
    _tsAlignH = Just alignH
  }
}

textAlignV :: AlignV -> Style
textAlignV alignV = mempty {
  _textStyle = Just $ mempty {
    _tsAlignV = Just alignV
  }
}


--makeLenses ''Style
--makeLenses ''TextStyle
--
--textAlignV :: AlignV -> Style
--textAlignV alignV = mempty & textStyle ?~ (mempty & tsAlignV ?~ alignV)

{--

minStyleSize = 0 :: Double
maxStyleSize = 100000 :: Double

data Unit = Px | Pct deriving (Show, Eq)

data SizeReq = SizeReq Double Double Unit deriving (Show, Eq)

instance Semigroup SizeReq where
  (<>) (SizeReq min1 max1 Px) (SizeReq min2 max2 Px) = SizeReq (max min1 min2) (max max1 max2) Px
  (<>) (SizeReq min1 max1 Pct) (SizeReq min2 max2 Pct) = SizeReq (max min1 min2) (max max1 max2) Pct
  (<>) s1@(SizeReq _ _ Px) _ = s1
  (<>) _ s2@(SizeReq _ _ Px) = s2

instance Monoid SizeReq where
  mempty = SizeReq 0 maxStyleSize Pct

minSizePx size = SizeReq size maxStyleSize Px
maxSizePx size = SizeReq minStyleSize size Px

minSizePct size = SizeReq size maxStyleSize Pct
maxSizePct size = SizeReq minStyleSize size Pct

data Style = Style {
  _sWidth     :: SizeReq,
  _sHeight    :: SizeReq,
  _sBgColor   :: C.Color,
  _sFont      :: T.Text,
  _sFontSize  :: Int,
  _sFontColor :: C.Color
}

instance Semigroup Style where
  (<>) s1 s2 = Style {
    _sWidth     = (_sWidth s1) <> (_sWidth s2),
    _sHeight    = (_sHeight s1) <> (_sHeight s2),
    _sBgColor   = (_sBgColor s2),
    _sFont      = (_sFont s2),
    _sFontSize  = (_sFontSize s2),
    _sFontColor = (_sFontColor s2)
  }

instance Monoid Style where
  mempty = Style mempty mempty C.black "sans" 16 C.white

data Stroke = Solid | Dashed deriving (Show, Eq)
data Unit = Px | Pct deriving (Show, Eq)
data Side = Top | Bottom | Left | Right | All deriving (Show, Eq)

data Corner = C_TL | C_TR | C_BL | C_BR | C_T | C_B | C_L | C_R | C_TBLR deriving (Show, Eq)

instance Semigroup Corner where
  (<>) _ b = b

instance Monoid Corner where
  mempty = C_TBLR

data Alignment = A_LT | A_LM | A_LB | A_CT | A_CM | A_CB | A_RT | A_RM | A_RB deriving (Show, Eq)

instance Semigroup Alignment where
  (<>) _ b = b

instance Monoid Alignment where
  mempty = A_LT

data SizeReq = SizeReq Double Double Unit deriving (Show, Eq)

instance Semigroup SizeReq where
  (<>) (SizeReq min1 max1 Px) (SizeReq min2 max2 Px) = SizeReq (max min1 min2) (max max1 max2) Px
  (<>) (SizeReq min1 max1 Pct) (SizeReq min2 max2 Pct) = SizeReq (max min1 min2) (max max1 max2) Pct
  (<>) s1@(SizeReq _ _ Px) _ = s1
  (<>) _ s2@(SizeReq _ _ Px) = s2

instance Monoid SizeReq where
  mempty = SizeReq 0 100000 Pct

data CornerShape = Normal | Straight | Rounded deriving (Show, Eq)

instance Semigroup CornerShape where
  (<>) Normal _ = Normal
  (<>) _ Normal = Normal
  (<>) cs1 _ = cs1

instance Monoid CornerShape where
  mempty = Normal

data CornerStyle = CornerStyle Corner CornerShape Double Unit deriving (Show, Eq)

instance Semigroup CornerStyle where
  (<>) _ b = b

instance Monoid CornerStyle where
  mempty = CornerStyle C_TBLR Normal 1 Px

data Border = Border {
  _bLeft   :: (Stroke, Double, Unit),
  _bRight  :: (Stroke, Double, Unit),
  _bTop    :: (Stroke, Double, Unit),
  _bBottom :: (Stroke, Double, Unit)
} deriving (Show, Eq)

instance Semigroup Border where
  (<>) (Border l1 r1 t1 b1) (Border l2 r2 t2 b2) = Border (bmax l1 l2) (bmax r1 r2) (bmax t1 t2) (bmax b1 b2) where
    bmax t1@(s1, v1, Px) t2@(s2, v2, Px) = if v1 > v2 then t1 else t2
    bmax t1@(s1, v1, Pct) t2@(s2, v2, Pct) = if v1 > v2 then t1 else t2
    bmax t1@(s1, v1, Px) _ = t1
    bmax _ t2@(s1, v1, Px) = t2

instance Monoid Border where
  mempty = Border ev ev ev ev where
    ev = (Solid, 0, Px)

data Padding = Padding {
  _pLeft   :: Double,
  _pRight  :: Double,
  _pTop    :: Double,
  _pBottom :: Double
} deriving (Show, Eq)

instance Semigroup Padding where
  (<>) (Padding l1 r1 t1 b1) (Padding l2 r2 t2 b2) = Padding (max l1 l2) (max r1 r2) (max t1 t2) (max b1 b2)

instance Monoid Padding where
  mempty = Padding 0 0 0 0

data Offset = Offset {
  _oLeft   :: Double,
  _oRight  :: Double,
  _oTop    :: Double,
  _oBottom :: Double
} deriving (Show, Eq)

instance Semigroup Offset where
  (<>) (Offset l1 r1 t1 b1) (Offset l2 r2 t2 b2) = Offset (max l1 l2) (max r1 r2) (max t1 t2) (max b1 b2)

instance Monoid Offset where
  mempty = Offset 0 0 0 0

--data Style = Width SizeReq | Height SizeReq | Padding [(Double, Side)] | Offset [(Double, Side)] | Border [Border] | Align [Alignment] | CornerStyle [CornerStyle]
--data StyleClass = String [Style]

data Style = Style {
  _sWidth     :: SizeReq,
  _sHeight    :: SizeReq,
  _sPadding   :: Padding,
  _sOffset    :: Offset,
  _sBorder    :: Border,
  _sAlignment :: Alignment,
  _sCorner    :: CornerStyle
}

instance Semigroup Style where
  (<>) = mappend

instance Monoid Style where
  mempty = Style mempty mempty mempty mempty mempty mempty mempty
  mappend (Style w1 h1 p1 o1 b1 a1 c1) (Style w2 h2 p2 o2 b2 a2 c2) = Style {
      _sWidth     = w1 <> w2,
      _sHeight    = h1 <> h2,
      _sPadding   = p1 <> p2,
      _sOffset    = o1 <> o2,
      _sBorder    = b1 <> b2,
      _sAlignment = a1 <> a2,
      _sCorner    = c1 <> c2
    }

--}
{--

STYLES

- Width
- Height
- Padding
- Offset
- Border
- Alignment
- Corners

--}
