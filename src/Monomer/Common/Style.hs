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
    _styleWidth = max (_styleWidth style2) (_styleWidth style1),
    _styleHeight = max (_styleHeight style2) (_styleHeight style1),
    _styleColor = _styleColor style2 <|> _styleColor style1,
    _styleHover = _styleHover style2 <|> _styleHover style1,
    _styleMargin = _styleMargin style2 <> _styleMargin style1,
    _stylePadding = _stylePadding style2 <> _stylePadding style1,
    _styleBorder = _styleBorder style2 <> _styleBorder style1,
    _styleRadius = _styleRadius style2 <> _styleRadius style1,
    _styleText = _styleText style2 <> _styleText style1
  }

instance Monoid Style where
  mempty = def

data Margin = Margin {
  _marginLeft :: Maybe Double,
  _marginRight :: Maybe Double,
  _marginTop :: Maybe Double,
  _marginBottom :: Maybe Double
} deriving (Show, Eq)

instance Default Margin where
  def = Margin {
    _marginLeft = Nothing,
    _marginRight = Nothing,
    _marginTop = Nothing,
    _marginBottom = Nothing
  }

instance Semigroup Margin where
  (<>) p1 p2 = Margin {
    _marginLeft = _marginLeft p2 <|> _marginLeft p1,
    _marginRight = _marginRight p2 <|> _marginRight p1,
    _marginTop = _marginTop p2 <|> _marginTop p1,
    _marginBottom = _marginBottom p2 <|> _marginBottom p1
  }

instance Monoid Margin where
  mempty = def

data Padding = Padding {
  _paddingLeft :: Maybe Double,
  _paddingRight :: Maybe Double,
  _paddingTop :: Maybe Double,
  _paddingBottom :: Maybe Double
} deriving (Show, Eq)

instance Default Padding where
  def = Padding {
    _paddingLeft = Nothing,
    _paddingRight = Nothing,
    _paddingTop = Nothing,
    _paddingBottom = Nothing
  }

instance Semigroup Padding where
  (<>) p1 p2 = Padding {
    _paddingLeft = _paddingLeft p2 <|> _paddingLeft p1,
    _paddingRight = _paddingRight p2 <|> _paddingRight p1,
    _paddingTop = _paddingTop p2 <|> _paddingTop p1,
    _paddingBottom = _paddingBottom p2 <|> _paddingBottom p1
  }

instance Monoid Padding where
  mempty = def

data BorderSide = BorderSide {
  _borderSideWidth :: Double,
  _borderSideColor :: Color
} deriving (Show, Eq)

instance Default BorderSide where
  def = BorderSide {
    _borderSideWidth = 0,
    _borderSideColor = def
  }

instance Semigroup BorderSide where
  (<>) _ b2 = b2

instance Monoid BorderSide where
  mempty = mempty

data Border = Border {
  _borderLeft :: Maybe BorderSide,
  _borderRight :: Maybe BorderSide,
  _borderTop :: Maybe BorderSide,
  _borderBottom :: Maybe BorderSide
} deriving (Show, Eq)

instance Default Border where
  def = Border {
    _borderLeft = def,
    _borderRight = def,
    _borderTop = def,
    _borderBottom = def
  }

instance Semigroup Border where
  (<>) b1 b2 = Border {
    _borderLeft = _borderLeft b2 <> _borderLeft b1,
    _borderRight = _borderRight b2 <> _borderRight b1,
    _borderTop = _borderTop b2 <> _borderTop b1,
    _borderBottom = _borderBottom b2 <> _borderBottom b1
  }

instance Monoid Border where
  mempty = def

data Radius = Radius {
  _radiusTopLeft :: Maybe Double,
  _radiusTopRight :: Maybe Double,
  _radiusBottomLeft :: Maybe Double,
  _radiusBottomRight :: Maybe Double
} deriving (Show, Eq)

instance Default Radius where
  def = Radius {
    _radiusTopLeft = Nothing,
    _radiusTopRight = Nothing,
    _radiusBottomLeft = Nothing,
    _radiusBottomRight = Nothing
  }

instance Semigroup Radius where
  (<>) r1 r2 = Radius {
    _radiusTopLeft = _radiusTopLeft r2 <|> _radiusTopLeft r1,
    _radiusTopRight = _radiusTopRight r2 <|> _radiusTopRight r1,
    _radiusBottomLeft = _radiusBottomLeft r2 <|> _radiusBottomLeft r1,
    _radiusBottomRight = _radiusBottomRight r2 <|> _radiusBottomRight r1
  }

instance Monoid Radius where
  mempty = def

data TextStyle = TextStyle {
  _textStyleFont :: Maybe String,
  _textStyleFontSize :: Maybe Double,
  _textStyleColor :: Maybe Color,
  _textStyleHover :: Maybe Color,
  _textStyleAlignH :: Maybe AlignH,
  _textStyleAlignV :: Maybe AlignV
} deriving (Show, Eq)

instance Default TextStyle where
  def = TextStyle {
    _textStyleFont = Nothing,
    _textStyleFontSize = Nothing,
    _textStyleColor = Nothing,
    _textStyleHover = Nothing,
    _textStyleAlignH = Nothing,
    _textStyleAlignV = Nothing
  }

instance Semigroup TextStyle where
  (<>) ts1 ts2 = TextStyle {
    _textStyleFont = _textStyleFont ts2 <|> _textStyleFont ts1,
    _textStyleFontSize = _textStyleFontSize ts2 <|> _textStyleFontSize ts1,
    _textStyleColor = _textStyleColor ts2 <|> _textStyleColor ts1,
    _textStyleHover = _textStyleHover ts2 <|> _textStyleHover ts1,
    _textStyleAlignH = _textStyleAlignH ts2 <|> _textStyleAlignH ts1,
    _textStyleAlignV = _textStyleAlignV ts2 <|> _textStyleAlignV ts1
  }

instance Monoid TextStyle where
  mempty = def
