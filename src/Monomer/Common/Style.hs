module Monomer.Common.Style where

import Control.Applicative
import Data.Default

import Monomer.Graphics.Types

-- | Basic styling attributes
data Style =
  Style {
    _styleWidth :: Maybe Double,
    _styleHeight :: Maybe Double,
    _styleColor :: Maybe Color,
    _styleHover :: Maybe Color,
    _styleMargin :: Maybe Margin,
    _stylePadding :: Maybe Padding,
    _styleBorder :: Maybe Border,
    _styleRadius :: Maybe Radius,
    _styleText :: Maybe TextStyle
  } deriving (Show, Eq)

instance Default Style where
  def = Style {
    _styleWidth = Nothing,
    _styleHeight = Nothing,
    _styleColor = Nothing,
    _styleHover = Nothing,
    _styleMargin = Nothing,
    _stylePadding = Nothing,
    _styleBorder = Nothing,
    _styleRadius = Nothing,
    _styleText = Nothing
  }

instance Semigroup Style where
  (<>) style1 style2 = Style {
    _styleWidth = _styleWidth style2 <|> _styleWidth style1,
    _styleHeight = _styleHeight style2 <|> _styleHeight style1,
    _styleColor = _styleColor style2 <|> _styleColor style1,
    _styleHover = _styleHover style2 <|> _styleHover style1,
    _styleMargin = _styleMargin style1 <> _styleMargin style2,
    _stylePadding = _stylePadding style1 <> _stylePadding style2,
    _styleBorder = _styleBorder style1 <> _styleBorder style2,
    _styleRadius = _styleRadius style1 <> _styleRadius style2,
    _styleText = _styleText style1 <> _styleText style2
  }

instance Monoid Style where
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
  _txsHover :: Maybe Color,
  _txsAlignH :: Maybe AlignH,
  _txsAlignV :: Maybe AlignV
} deriving (Show, Eq)

instance Default TextStyle where
  def = TextStyle {
    _txsFont = Nothing,
    _txsFontSize = Nothing,
    _txsColor = Nothing,
    _txsHover = Nothing,
    _txsAlignH = Nothing,
    _txsAlignV = Nothing
  }

instance Semigroup TextStyle where
  (<>) ts1 ts2 = TextStyle {
    _txsFont = _txsFont ts2 <|> _txsFont ts1,
    _txsFontSize = _txsFontSize ts2 <|> _txsFontSize ts1,
    _txsColor = _txsColor ts2 <|> _txsColor ts1,
    _txsHover = _txsHover ts2 <|> _txsHover ts1,
    _txsAlignH = _txsAlignH ts2 <|> _txsAlignH ts1,
    _txsAlignV = _txsAlignV ts2 <|> _txsAlignV ts1
  }

instance Monoid TextStyle where
  mempty = def
