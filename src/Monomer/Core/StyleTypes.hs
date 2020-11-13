module Monomer.Core.StyleTypes where

import Control.Applicative ((<|>))
import Data.Default

import Monomer.Core.BasicTypes
import Monomer.Graphics.Color
import Monomer.Graphics.Types

-- | Basic styling attributes
data SizeReq
  = FixedSize Double
  | FlexSize Double Factor
  | MinSize Double Factor
  | MaxSize Double Factor
  | RangeSize Double Double Factor
  deriving (Eq, Show)

instance Default SizeReq where
  def = FlexSize 0 1

data CursorIcon
  = CursorArrow
  | CursorHand
  | CursorIBeam
  | CursorInvalid
  | CursorSizeH
  | CursorSizeV
  | CursorDiagTL
  | CursorDiagTR
  deriving (Eq, Ord, Enum, Show)

data Style = Style {
  _styleBasic :: Maybe StyleState,
  _styleHover :: Maybe StyleState,
  _styleFocus :: Maybe StyleState,
  _styleDisabled :: Maybe StyleState
} deriving (Eq, Show)

instance Default Style where
  def = Style {
    _styleBasic = Nothing,
    _styleHover = Nothing,
    _styleFocus = Nothing,
    _styleDisabled = Nothing
  }

instance Semigroup Style where
  (<>) style1 style2 = Style {
    _styleBasic = _styleBasic style1 <> _styleBasic style2,
    _styleHover = _styleHover style1 <> _styleHover style2,
    _styleFocus = _styleFocus style1 <> _styleFocus style2,
    _styleDisabled = _styleDisabled style1 <> _styleDisabled style2
  }

instance Monoid Style where
  mempty = def

data StyleState = StyleState {
  _sstSizeReqW :: Maybe SizeReq,
  _sstSizeReqH :: Maybe SizeReq,
  _sstPadding :: Maybe Padding,
  _sstBorder :: Maybe Border,
  _sstRadius :: Maybe Radius,
  _sstBgColor :: Maybe Color,
  _sstFgColor :: Maybe Color,
  _sstHlColor :: Maybe Color,
  _sstText :: Maybe TextStyle,
  _sstCursorIcon :: Maybe CursorIcon
} deriving (Eq, Show)

instance Default StyleState where
  def = StyleState {
    _sstSizeReqW = Nothing,
    _sstSizeReqH = Nothing,
    _sstPadding = Nothing,
    _sstBorder = Nothing,
    _sstRadius = Nothing,
    _sstBgColor = Nothing,
    _sstFgColor = Nothing,
    _sstHlColor = Nothing,
    _sstText = Nothing,
    _sstCursorIcon = Nothing
  }

instance Semigroup StyleState where
  (<>) s1 s2 = StyleState {
    _sstSizeReqW = _sstSizeReqW s2 <|> _sstSizeReqW s1,
    _sstSizeReqH = _sstSizeReqH s2 <|> _sstSizeReqH s1,
    _sstPadding = _sstPadding s1 <> _sstPadding s2,
    _sstBorder = _sstBorder s1 <> _sstBorder s2,
    _sstRadius = _sstRadius s1 <> _sstRadius s2,
    _sstBgColor = _sstBgColor s2 <|> _sstBgColor s1,
    _sstFgColor = _sstFgColor s2 <|> _sstFgColor s1,
    _sstHlColor = _sstHlColor s2 <|> _sstHlColor s1,
    _sstText = _sstText s1 <> _sstText s2,
    _sstCursorIcon = _sstCursorIcon s2 <|> _sstCursorIcon s1
  }

instance Monoid StyleState where
  mempty = def

data Padding = Padding {
  _padLeft :: Maybe Double,
  _padRight :: Maybe Double,
  _padTop :: Maybe Double,
  _padBottom :: Maybe Double
} deriving (Eq, Show)

instance Default Padding where
  def = Padding {
    _padLeft = Nothing,
    _padRight = Nothing,
    _padTop = Nothing,
    _padBottom = Nothing
  }

instance Semigroup Padding where
  (<>) p1 p2 = Padding {
    _padLeft = _padLeft p2 <|> _padLeft p1,
    _padRight = _padRight p2 <|> _padRight p1,
    _padTop = _padTop p2 <|> _padTop p1,
    _padBottom = _padBottom p2 <|> _padBottom p1
  }

instance Monoid Padding where
  mempty = def

data BorderSide = BorderSide {
  _bsWidth :: Double,
  _bsColor :: Color
} deriving (Eq, Show)

instance Default BorderSide where
  def = BorderSide {
    _bsWidth = 0,
    _bsColor = def
  }

instance Semigroup BorderSide where
  (<>) b1 b2 = b2

instance Monoid BorderSide where
  mempty = def

data Border = Border {
  _brdLeft :: Maybe BorderSide,
  _brdRight :: Maybe BorderSide,
  _brdTop :: Maybe BorderSide,
  _brdBottom :: Maybe BorderSide
} deriving (Eq, Show)

instance Default Border where
  def = Border {
    _brdLeft = def,
    _brdRight = def,
    _brdTop = def,
    _brdBottom = def
  }

instance Semigroup Border where
  (<>) b1 b2 = Border {
    _brdLeft = _brdLeft b1 <> _brdLeft b2,
    _brdRight = _brdRight b1 <> _brdRight b2,
    _brdTop = _brdTop b1 <> _brdTop b2,
    _brdBottom = _brdBottom b1 <> _brdBottom b2
  }

instance Monoid Border where
  mempty = def

data RadiusType
  = RadiusInner
  | RadiusBoth
  deriving (Eq, Show)

instance Default RadiusType where
  def = RadiusBoth

instance Semigroup RadiusType where
  (<>) rc1 rc2 = rc2

instance Monoid RadiusType where
  mempty = def

data RadiusCorner = RadiusCorner {
  _rcrType :: RadiusType,
  _rcrWidth :: Double
} deriving (Eq, Show)

instance Default RadiusCorner where
  def = RadiusCorner {
    _rcrType = RadiusBoth,
    _rcrWidth = def
  }

instance Semigroup RadiusCorner where
  (<>) rc1 rc2 = rc2

instance Monoid RadiusCorner where
  mempty = def

data Radius = Radius {
  _radTopLeft :: Maybe RadiusCorner,
  _radTopRight :: Maybe RadiusCorner,
  _radBottomLeft :: Maybe RadiusCorner,
  _radBottomRight :: Maybe RadiusCorner
} deriving (Eq, Show)

instance Default Radius where
  def = Radius {
    _radTopLeft = Nothing,
    _radTopRight = Nothing,
    _radBottomLeft = Nothing,
    _radBottomRight = Nothing
  }

instance Semigroup Radius where
  (<>) r1 r2 = Radius {
    _radTopLeft = _radTopLeft r2 <|> _radTopLeft r1,
    _radTopRight = _radTopRight r2 <|> _radTopRight r1,
    _radBottomLeft = _radBottomLeft r2 <|> _radBottomLeft r1,
    _radBottomRight = _radBottomRight r2 <|> _radBottomRight r1
  }

instance Monoid Radius where
  mempty = def

data TextStyle = TextStyle {
  _txsFont :: Maybe Font,
  _txsFontSize :: Maybe FontSize,
  _txsFontColor :: Maybe Color,
  _txsAlignH :: Maybe AlignH,
  _txsAlignV :: Maybe AlignV
} deriving (Eq, Show)

instance Default TextStyle where
  def = TextStyle {
    _txsFont = Nothing,
    _txsFontSize = Nothing,
    _txsFontColor = Nothing,
    _txsAlignH = Nothing,
    _txsAlignV = Nothing
  }

instance Semigroup TextStyle where
  (<>) ts1 ts2 = TextStyle {
    _txsFont = _txsFont ts2 <|> _txsFont ts1,
    _txsFontSize = _txsFontSize ts2 <|> _txsFontSize ts1,
    _txsFontColor = _txsFontColor ts2 <|> _txsFontColor ts1,
    _txsAlignH = _txsAlignH ts2 <|> _txsAlignH ts1,
    _txsAlignV = _txsAlignV ts2 <|> _txsAlignV ts1
  }

instance Monoid TextStyle where
  mempty = def
