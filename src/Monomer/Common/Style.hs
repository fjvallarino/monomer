module Monomer.Common.Style where

import Control.Applicative
import Data.Default

import Monomer.Graphics.Types

data Theme = Theme {
  _themeBasic :: ThemeState,
  _themeHover :: ThemeState,
  _themeFocus :: ThemeState
} deriving (Show, Eq)

instance Default Theme where
  def = Theme {
    _themeBasic = def,
    _themeHover = def,
    _themeFocus = def
  }

instance Semigroup Theme where
  (<>) t1 t2 = t2

instance Monoid Theme where
  mempty = def

data ThemeState = ThemeState {
  _thsFgColor :: Color,
  _thsFont :: Font,
  _thsFontSize :: FontSize,
  _thsColor :: Color
} deriving (Show, Eq)

instance Default ThemeState where
  def = ThemeState {
    _thsFgColor = Color 255 255 255 1,
    _thsFont = Font "sans",
    _thsFontSize = FontSize 36,
    _thsColor = Color 255 255 255 1
  }

instance Semigroup ThemeState where
  (<>) ts1 ts2 = ts2

instance Monoid ThemeState where
  mempty = def

-- | Basic styling attributes
data Style = Style {
  _styleBasic :: Maybe StyleState,
  _styleHover :: Maybe StyleState,
  _styleFocus :: Maybe StyleState
} deriving (Show, Eq)

instance Default Style where
  def = Style {
    _styleBasic = Nothing,
    _styleHover = Nothing,
    _styleFocus = Nothing
  }

instance Semigroup Style where
  (<>) style1 style2 = Style {
    _styleBasic = _styleBasic style1 <> _styleBasic style2,
    _styleHover = _styleHover style1 <> _styleHover style2,
    _styleFocus = _styleFocus style1 <> _styleFocus style2
  }

instance Monoid Style where
  mempty = def

data StyleState = StyleState {
  _sstWidth :: Maybe Double,
  _sstHeight :: Maybe Double,
  _sstMargin :: Maybe Margin,
  _sstPadding :: Maybe Padding,
  _sstBorder :: Maybe Border,
  _sstRadius :: Maybe Radius,
  _sstBgColor :: Maybe Color,
  _sstFgColor :: Maybe Color,
  _sstText :: Maybe TextStyle
} deriving (Show, Eq)

instance Default StyleState where
  def = StyleState {
    _sstWidth = Nothing,
    _sstHeight = Nothing,
    _sstMargin = Nothing,
    _sstPadding = Nothing,
    _sstBorder = Nothing,
    _sstRadius = Nothing,
    _sstBgColor = Nothing,
    _sstFgColor = Nothing,
    _sstText = Nothing
  }

instance Semigroup StyleState where
  (<>) s1 s2 = StyleState {
    _sstWidth = _sstWidth s2 <|> _sstWidth s1,
    _sstHeight = _sstHeight s2 <|> _sstHeight s1,
    _sstMargin = _sstMargin s1 <> _sstMargin s2,
    _sstPadding = _sstPadding s1 <> _sstPadding s2,
    _sstBorder = _sstBorder s1 <> _sstBorder s2,
    _sstRadius = _sstRadius s1 <> _sstRadius s2,
    _sstBgColor = _sstBgColor s1 <> _sstBgColor s2,
    _sstFgColor = _sstFgColor s1 <> _sstFgColor s2,
    _sstText = _sstText s1 <> _sstText s2
  }

instance Monoid StyleState where
  mempty = def

data Margin = Margin {
  _mgnLeft :: Maybe Double,
  _mgnRight :: Maybe Double,
  _mgnTop :: Maybe Double,
  _mgnBottom :: Maybe Double
} deriving (Show, Eq)

instance Default Margin where
  def = Margin {
    _mgnLeft = Nothing,
    _mgnRight = Nothing,
    _mgnTop = Nothing,
    _mgnBottom = Nothing
  }

instance Semigroup Margin where
  (<>) p1 p2 = Margin {
    _mgnLeft = _mgnLeft p2 <|> _mgnLeft p1,
    _mgnRight = _mgnRight p2 <|> _mgnRight p1,
    _mgnTop = _mgnTop p2 <|> _mgnTop p1,
    _mgnBottom = _mgnBottom p2 <|> _mgnBottom p1
  }

instance Monoid Margin where
  mempty = def

data Padding = Padding {
  _padLeft :: Maybe Double,
  _padRight :: Maybe Double,
  _padTop :: Maybe Double,
  _padBottom :: Maybe Double
} deriving (Show, Eq)

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
} deriving (Show, Eq)

instance Default BorderSide where
  def = BorderSide {
    _bsWidth = 0,
    _bsColor = def
  }

instance Semigroup BorderSide where
  (<>) b1 b2 = b2

instance Monoid BorderSide where
  mempty = mempty

data Border = Border {
  _brdLeft :: Maybe BorderSide,
  _brdRight :: Maybe BorderSide,
  _brdTop :: Maybe BorderSide,
  _brdBottom :: Maybe BorderSide
} deriving (Show, Eq)

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

data Radius = Radius {
  _radTopLeft :: Maybe Double,
  _radTopRight :: Maybe Double,
  _radBottomLeft :: Maybe Double,
  _radBottomRight :: Maybe Double
} deriving (Show, Eq)

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
  _txsColor :: Maybe Color,
  _txsAlignH :: Maybe AlignH,
  _txsAlignV :: Maybe AlignV
} deriving (Show, Eq)

instance Default TextStyle where
  def = TextStyle {
    _txsFont = Nothing,
    _txsFontSize = Nothing,
    _txsColor = Nothing,
    _txsAlignH = Nothing,
    _txsAlignV = Nothing
  }

instance Semigroup TextStyle where
  (<>) ts1 ts2 = TextStyle {
    _txsFont = _txsFont ts2 <|> _txsFont ts1,
    _txsFontSize = _txsFontSize ts2 <|> _txsFontSize ts1,
    _txsColor = _txsColor ts2 <|> _txsColor ts1,
    _txsAlignH = _txsAlignH ts2 <|> _txsAlignH ts1,
    _txsAlignV = _txsAlignV ts2 <|> _txsAlignV ts1
  }

instance Monoid TextStyle where
  mempty = def
