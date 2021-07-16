{-|
Module      : Monomer.Core.StyleTypes
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Basic types for styling widgets.
-}
{-# LANGUAGE DeriveGeneric #-}

module Monomer.Core.StyleTypes where

import Control.Applicative ((<|>))
import Data.Default
import GHC.Generics

import Monomer.Common
import Monomer.Graphics.Types
import Monomer.Graphics.Util

{-|
Represents a size requirement for a specific axis. Mainly used by stack and box,
with grid using it as the base for its calculations. Each field represents:

- Fixed: A minimum size required by the widget. This type of space is the first
that gets assigned.
- Flex: Additional space the widget accepts, up to the provided value. After
fixed requirements are satisfied, flex sizes are assigned proportionally
considering factor.
- Extra: After flex is satisfied, the remaining space is distributed
proportionally, considering factor, to all non zero extra requirements. There is
no limit to how much extra space can be assigned.
- Factor: How much flex/extra space a widget will get proportionally. This also
affects how much a requirement is willing to lose: a value less than 1 can
receive less space, but gives up less too.
-}
data SizeReq = SizeReq {
  _szrFixed :: Double,
  _szrFlex :: Double,
  _szrExtra :: Double,
  _szrFactor :: Factor
} deriving (Eq, Show, Generic)

instance Default SizeReq where
  def = SizeReq {
    _szrFixed = 0,
    _szrFlex = 0,
    _szrExtra = 0,
    _szrFactor = 1
  }

-- | Different mouse pointer types.
data CursorIcon
  = CursorArrow
  | CursorHand
  | CursorIBeam
  | CursorInvalid
  | CursorSizeH
  | CursorSizeV
  | CursorDiagTL
  | CursorDiagTR
  deriving (Eq, Ord, Enum, Show, Generic)

instance Default CursorIcon where
  def = CursorArrow

{-|
Main style type, comprised of configurations for the different states:

- Basic: Starting state for a widget, without any kind of interaction. This
is used as the base for all other states, which override values as needed.
- Hover: The mouse pointer is on top of the current widget.
- Focus: The widget has keyboard focus.
- Focus-Hover: The widget has keyboard focus and mouse is on top. Without this
state one of Hover or Focus would take precedence and it would not be possible
to specify the desired behavior.
- Active: The mouse button is currently presed and the pointer is within the
boundaries of the widget.
- Disabled: The widget is disabled.
-}
data Style = Style {
  _styleBasic :: Maybe StyleState,
  _styleHover :: Maybe StyleState,
  _styleFocus :: Maybe StyleState,
  _styleFocusHover :: Maybe StyleState,
  _styleActive :: Maybe StyleState,
  _styleDisabled :: Maybe StyleState
} deriving (Eq, Show, Generic)

instance Default Style where
  def = Style {
    _styleBasic = Nothing,
    _styleHover = Nothing,
    _styleFocus = Nothing,
    _styleFocusHover = Nothing,
    _styleActive = Nothing,
    _styleDisabled = Nothing
  }

instance Semigroup Style where
  (<>) style1 style2 = Style {
    _styleBasic = _styleBasic style1 <> _styleBasic style2,
    _styleHover = _styleHover style1 <> _styleHover style2,
    _styleFocus = _styleFocus style1 <> _styleFocus style2,
    _styleFocusHover = _styleFocusHover style1 <> _styleFocusHover style2,
    _styleActive = _styleActive style1 <> _styleActive style2,
    _styleDisabled = _styleDisabled style1 <> _styleDisabled style2
  }

instance Monoid Style where
  mempty = def

{-|
Customizable style items for a specific state. All values are optional, and can
be combined with the latest values taking precedence when the previous value is
not empty.
-}
data StyleState = StyleState {
  -- | User defined width req. Takes precedence over widget req.
  _sstSizeReqW :: Maybe SizeReq,
  -- | User defined height req. Takes precedence over widget req.
  _sstSizeReqH :: Maybe SizeReq,
  -- | Space between the border and the content of the widget
  _sstPadding :: Maybe Padding,
  -- | Border definition.
  _sstBorder :: Maybe Border,
  -- | Radius. Affects both border and background.
  _sstRadius :: Maybe Radius,
  -- | Background color.
  _sstBgColor :: Maybe Color,
  -- | Main foreground color. Each widget decides how it uses it.
  _sstFgColor :: Maybe Color,
  -- | Secondary foreground color. Each widget decides how it uses it.
  _sstSndColor :: Maybe Color,
  -- | Highlight color. Each widget decides how it uses it.
  _sstHlColor :: Maybe Color,
  -- | Text style, including font, size and color.
  _sstText :: Maybe TextStyle,
  -- | The assigned cursor icon to this specific state.
  _sstCursorIcon :: Maybe CursorIcon
} deriving (Eq, Show, Generic)

instance Default StyleState where
  def = StyleState {
    _sstSizeReqW = Nothing,
    _sstSizeReqH = Nothing,
    _sstPadding = Nothing,
    _sstBorder = Nothing,
    _sstRadius = Nothing,
    _sstBgColor = Nothing,
    _sstFgColor = Nothing,
    _sstSndColor = Nothing,
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
    _sstSndColor = _sstSndColor s2 <|> _sstSndColor s1,
    _sstHlColor = _sstHlColor s2 <|> _sstHlColor s1,
    _sstText = _sstText s1 <> _sstText s2,
    _sstCursorIcon = _sstCursorIcon s2 <|> _sstCursorIcon s1
  }

instance Monoid StyleState where
  mempty = def

-- | Padding definitions (space between border and content) for each side.
data Padding = Padding {
  _padLeft :: Maybe Double,
  _padRight :: Maybe Double,
  _padTop :: Maybe Double,
  _padBottom :: Maybe Double
} deriving (Eq, Show, Generic)

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

-- | Defines width and color for a given border side.
data BorderSide = BorderSide {
  _bsWidth :: Double,
  _bsColor :: Color
} deriving (Eq, Show, Generic)

instance Default BorderSide where
  def = BorderSide {
    _bsWidth = 0,
    _bsColor = def
  }

instance Semigroup BorderSide where
  (<>) b1 b2 = b2

instance Monoid BorderSide where
  mempty = def

-- Border definitions for each side.
data Border = Border {
  _brdLeft :: Maybe BorderSide,
  _brdRight :: Maybe BorderSide,
  _brdTop :: Maybe BorderSide,
  _brdBottom :: Maybe BorderSide
} deriving (Eq, Show, Generic)

instance Default Border where
  def = Border {
    _brdLeft = Nothing,
    _brdRight = Nothing,
    _brdTop = Nothing,
    _brdBottom = Nothing
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
  deriving (Eq, Show, Generic)

instance Default RadiusType where
  def = RadiusBoth

instance Semigroup RadiusType where
  (<>) rc1 rc2 = rc2

instance Monoid RadiusType where
  mempty = def

-- | Defines radius type and width/radius for a given corner.
newtype RadiusCorner = RadiusCorner {
  _rcrWidth :: Double
} deriving (Eq, Show, Generic)

instance Default RadiusCorner where
  def = RadiusCorner {
    _rcrWidth = def
  }

instance Semigroup RadiusCorner where
  (<>) rc1 rc2 = rc2

instance Monoid RadiusCorner where
  mempty = def

-- | Provides radius definitions for each corner.
data Radius = Radius {
  _radTopLeft :: Maybe RadiusCorner,
  _radTopRight :: Maybe RadiusCorner,
  _radBottomLeft :: Maybe RadiusCorner,
  _radBottomRight :: Maybe RadiusCorner
} deriving (Eq, Show, Generic)

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

-- | Text related definitions.
data TextStyle = TextStyle {
  _txsFont :: Maybe Font,          -- ^ The font type.
  _txsFontSize :: Maybe FontSize,  -- ^ Text size in pixels.
  _txsFontSpaceH :: Maybe FontSpace, -- ^ Horizontal text spacing in pixels.
  _txsFontSpaceV :: Maybe FontSpace, -- ^ Vertical text spacing in pixels.
  _txsFontColor :: Maybe Color,    -- ^ Text color.
  _txsUnderline :: Maybe Bool,     -- ^ True if underline should be displayed.
  _txsOverline :: Maybe Bool,      -- ^ True if overline should be displayed.
  _txsThroughline :: Maybe Bool,   -- ^ True if throughline should be displayed.
  _txsAlignH :: Maybe AlignTH,     -- ^ Horizontal alignment.
  _txsAlignV :: Maybe AlignTV      -- ^ Vertical alignment.
} deriving (Eq, Show, Generic)

instance Default TextStyle where
  def = TextStyle {
    _txsFont = Nothing,
    _txsFontSize = Nothing,
    _txsFontSpaceH = Nothing,
    _txsFontSpaceV = Nothing,
    _txsFontColor = Nothing,
    _txsUnderline = Nothing,
    _txsOverline = Nothing,
    _txsThroughline = Nothing,
    _txsAlignH = Nothing,
    _txsAlignV = Nothing
  }

instance Semigroup TextStyle where
  (<>) ts1 ts2 = TextStyle {
    _txsFont = _txsFont ts2 <|> _txsFont ts1,
    _txsFontSize = _txsFontSize ts2 <|> _txsFontSize ts1,
    _txsFontSpaceH = _txsFontSpaceH ts2 <|> _txsFontSpaceH ts1,
    _txsFontSpaceV = _txsFontSpaceV ts2 <|> _txsFontSpaceV ts1,
    _txsFontColor = _txsFontColor ts2 <|> _txsFontColor ts1,
    _txsUnderline = _txsUnderline ts2 <|> _txsUnderline ts1,
    _txsOverline = _txsOverline ts2 <|> _txsOverline ts1,
    _txsThroughline = _txsThroughline ts2 <|> _txsThroughline ts1,
    _txsAlignH = _txsAlignH ts2 <|> _txsAlignH ts1,
    _txsAlignV = _txsAlignV ts2 <|> _txsAlignV ts1
  }

instance Monoid TextStyle where
  mempty = def
