{-|
Module      : Monomer.Widgets.Singles.DateField
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Input field for dates types with support for valid ranges, different formats and
separators.

@
dateField dateLens
@

With configuration options:

@
dateField_ dateLens [dateFormatMMDDYYYY, dateFormatDelimiter \'-\']
@

Supports the 'Day' type of the <https://hackage.haskell.org/package/time time>
library, but other types can be supported by implementing 'DayConverter'.
'Maybe' is also supported.

Handles mouse wheel and shift + vertical drag to increase/decrease days.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Monomer.Widgets.Singles.DateField (
  -- * Configuration
  DateFieldCfg,
  DateFieldFormat,
  FormattableDate,
  DayConverter(..),
  DateTextConverter(..),
  -- * Constructors
  dateField,
  dateField_,
  dateFieldV,
  dateFieldV_,
  dateFieldD_,
  dateFormatDelimiter,
  dateFormatDDMMYYYY,
  dateFormatMMDDYYYY,
  dateFormatYYYYMMDD
) where

import Control.Applicative ((<|>))
import Control.Lens ((^.), ALens', _1, _2, _3)
import Control.Monad (join)
import Data.Default
import Data.Either
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Data.Typeable (Typeable, typeOf)
import TextShow

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event.Types
import Monomer.Widgets.Singles.Base.InputField

import qualified Monomer.Lens as L
import qualified Monomer.Widgets.Util.Parser as P

-- | Available formats for 'dateField'.
data DateFieldFormat
  = FormatDDMMYYYY
  | FormatYYYYMMDD
  | FormatMMDDYYYY
  deriving (Eq, Show)

defaultDateFormat :: DateFieldFormat
defaultDateFormat = FormatDDMMYYYY

defaultDateDelim :: Char
defaultDateDelim = '/'

{-|
Converter to and form the Day type of the time library. To use types other than
Day of said library, this typeclass needs to be implemented.
--}
class (Eq a, Ord a, Show a, Typeable a) => DayConverter a where
  convertFromDay :: Day -> a
  convertToDay :: a -> Maybe Day

instance DayConverter Day where
  convertFromDay = id
  convertToDay = Just

{-|
Converts a 'Day' instance to and from 'Text'. Implementing this typeclass
is not necessary for instances of 'DayConverter'.
-}
class DateTextConverter a where
  dateAcceptText :: DateFieldFormat -> Char -> Maybe a -> Maybe a -> Text -> (Bool, Bool, Maybe a)
  dateFromText :: DateFieldFormat -> Char -> Text -> Maybe a
  dateToText :: DateFieldFormat -> Char -> a -> Text
  dateFromDay :: Day -> a
  dateToDay :: a -> Maybe Day

instance {-# OVERLAPPABLE #-} DayConverter a => DateTextConverter a where
  dateAcceptText format delim minVal maxVal text = result where
    accept = acceptTextInput format delim text
    parsed = dateFromText format delim text
    isValid = isJust parsed && dateInBounds minVal maxVal (fromJust parsed)
    fromText
      | isValid = parsed
      | otherwise = Nothing
    result = (accept, isValid, fromText)
  dateFromText = dateFromTextSimple
  dateToText = dateToTextSimple
  dateFromDay = convertFromDay
  dateToDay = convertToDay

instance (DayConverter a, DateTextConverter a) => DateTextConverter (Maybe a) where
  dateAcceptText format delim minVal maxVal text
    | T.strip text == "" = (True, True, Just Nothing)
    | otherwise = (accept, isValid, result) where
      resp = dateAcceptText format delim (join minVal) (join maxVal) text
      (accept, isValid, tmpResult) = resp
      result
        | isJust tmpResult = Just tmpResult
        | otherwise = Nothing
  dateFromText format delim = Just . dateFromText format delim
  dateToText format delim Nothing = ""
  dateToText format delim (Just value) = dateToText format delim value
  dateFromDay = Just . dateFromDay
  dateToDay Nothing = Nothing
  dateToDay (Just value) = dateToDay value

-- | Constraints for date types accepted by dateField.
type FormattableDate a
  = (Eq a, Ord a, Show a, DateTextConverter a, Typeable a)

{-|
Configuration options for dateField:

- 'validInput': field indicating if the current input is valid. Useful to show
  warnings in the UI, or disable buttons if needed.
- 'resizeOnChange': Whether input causes 'ResizeWidgets' requests.
- 'selectOnFocus': Whether all input should be selected when focus is received.
- 'readOnly': Whether to prevent the user changing the input text.
- 'minValue': Minimum valid date.
- 'maxValue': Maximum valid date.
- 'wheelRate': The rate at which wheel movement affects the date.
- 'dragRate': The rate at which drag movement affects the date.
- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onChange': event to raise when the value changes.
- 'onChangeReq': 'WidgetRequest' to generate when the value changes.
- 'dateFormatDelimiter': which text delimiter to separate year, month and day.
- 'dateFormatDDMMYYYY': using the current delimiter, accept DD/MM/YYYY.
- 'dateFormatMMDDYYYY': using the current delimiter, accept MM/DD/YYYY.
- 'dateFormatYYYYMMDD': using the current delimiter, accept YYYY/MM/DD.
-}
data DateFieldCfg s e a = DateFieldCfg {
  _dfcCaretWidth :: Maybe Double,
  _dfcCaretMs :: Maybe Millisecond,
  _dfcValid :: Maybe (WidgetData s Bool),
  _dfcValidV :: [Bool -> e],
  _dfcDateDelim :: Maybe Char,
  _dfcDateFormat :: Maybe DateFieldFormat,
  _dfcMinValue :: Maybe a,
  _dfcMaxValue :: Maybe a,
  _dfcWheelRate :: Maybe Double,
  _dfcDragRate :: Maybe Double,
  _dfcResizeOnChange :: Maybe Bool,
  _dfcSelectOnFocus :: Maybe Bool,
  _dfcReadOnly :: Maybe Bool,
  _dfcOnFocusReq :: [Path -> WidgetRequest s e],
  _dfcOnBlurReq :: [Path -> WidgetRequest s e],
  _dfcOnChangeReq :: [a -> WidgetRequest s e]
}

instance Default (DateFieldCfg s e a) where
  def = DateFieldCfg {
    _dfcCaretWidth = Nothing,
    _dfcCaretMs = Nothing,
    _dfcValid = Nothing,
    _dfcValidV = [],
    _dfcDateDelim = Nothing,
    _dfcDateFormat = Nothing,
    _dfcMinValue = Nothing,
    _dfcMaxValue = Nothing,
    _dfcWheelRate = Nothing,
    _dfcDragRate = Nothing,
    _dfcResizeOnChange = Nothing,
    _dfcSelectOnFocus = Nothing,
    _dfcReadOnly = Nothing,
    _dfcOnFocusReq = [],
    _dfcOnBlurReq = [],
    _dfcOnChangeReq = []
  }

instance Semigroup (DateFieldCfg s e a) where
  (<>) t1 t2 = DateFieldCfg {
    _dfcCaretWidth = _dfcCaretWidth t2 <|> _dfcCaretWidth t1,
    _dfcCaretMs = _dfcCaretMs t2 <|> _dfcCaretMs t1,
    _dfcValid = _dfcValid t2 <|> _dfcValid t1,
    _dfcValidV = _dfcValidV t1 <> _dfcValidV t2,
    _dfcDateDelim = _dfcDateDelim t2 <|> _dfcDateDelim t1,
    _dfcDateFormat = _dfcDateFormat t2 <|> _dfcDateFormat t1,
    _dfcMinValue = _dfcMinValue t2 <|> _dfcMinValue t1,
    _dfcMaxValue = _dfcMaxValue t2 <|> _dfcMaxValue t1,
    _dfcWheelRate = _dfcWheelRate t2 <|> _dfcWheelRate t1,
    _dfcDragRate = _dfcDragRate t2 <|> _dfcDragRate t1,
    _dfcResizeOnChange = _dfcResizeOnChange t2 <|> _dfcResizeOnChange t1,
    _dfcSelectOnFocus = _dfcSelectOnFocus t2 <|> _dfcSelectOnFocus t1,
    _dfcReadOnly = _dfcReadOnly t2 <|> _dfcReadOnly t1,
    _dfcOnFocusReq = _dfcOnFocusReq t1 <> _dfcOnFocusReq t2,
    _dfcOnBlurReq = _dfcOnBlurReq t1 <> _dfcOnBlurReq t2,
    _dfcOnChangeReq = _dfcOnChangeReq t1 <> _dfcOnChangeReq t2
  }

instance Monoid (DateFieldCfg s e a) where
  mempty = def

instance CmbCaretWidth (DateFieldCfg s e a) Double where
  caretWidth w = def {
    _dfcCaretWidth = Just w
  }

instance CmbCaretMs (DateFieldCfg s e a) Millisecond where
  caretMs ms = def {
    _dfcCaretMs = Just ms
  }

instance CmbValidInput (DateFieldCfg s e a) s where
  validInput field = def {
    _dfcValid = Just (WidgetLens field)
  }

instance CmbValidInputV (DateFieldCfg s e a) e where
  validInputV fn = def {
    _dfcValidV = [fn]
  }

instance CmbResizeOnChange (DateFieldCfg s e a) where
  resizeOnChange_ resize = def {
    _dfcResizeOnChange = Just resize
  }

instance CmbSelectOnFocus (DateFieldCfg s e a) where
  selectOnFocus_ sel = def {
    _dfcSelectOnFocus = Just sel
  }

instance CmbReadOnly (DateFieldCfg s e a) where
  readOnly_ ro = def {
    _dfcReadOnly = Just ro
  }

instance FormattableDate a => CmbMinValue (DateFieldCfg s e a) a where
  minValue value = def {
    _dfcMinValue = Just value
  }

instance FormattableDate a => CmbMaxValue (DateFieldCfg s e a) a where
  maxValue value = def {
    _dfcMaxValue = Just value
  }

instance CmbWheelRate (DateFieldCfg s e a) Double where
  wheelRate rate = def {
    _dfcWheelRate = Just rate
  }

instance CmbDragRate (DateFieldCfg s e a) Double where
  dragRate rate = def {
    _dfcDragRate = Just rate
  }

instance WidgetEvent e => CmbOnFocus (DateFieldCfg s e a) e Path where
  onFocus fn = def {
    _dfcOnFocusReq = [RaiseEvent . fn]
  }

instance CmbOnFocusReq (DateFieldCfg s e a) s e Path where
  onFocusReq req = def {
    _dfcOnFocusReq = [req]
  }

instance WidgetEvent e => CmbOnBlur (DateFieldCfg s e a) e Path where
  onBlur fn = def {
    _dfcOnBlurReq = [RaiseEvent . fn]
  }

instance CmbOnBlurReq (DateFieldCfg s e a) s e Path where
  onBlurReq req = def {
    _dfcOnBlurReq = [req]
  }

instance WidgetEvent e => CmbOnChange (DateFieldCfg s e a) a e where
  onChange fn = def {
    _dfcOnChangeReq = [RaiseEvent . fn]
  }

instance CmbOnChangeReq (DateFieldCfg s e a) s e a where
  onChangeReq req = def {
    _dfcOnChangeReq = [req]
  }

-- | Which character should be used to delimit dates.
dateFormatDelimiter :: Char -> DateFieldCfg s e a
dateFormatDelimiter delim = def {
  _dfcDateDelim = Just delim
}

-- | Date format DD/MM/YYYY, using the appropriate delimiter.
dateFormatDDMMYYYY :: DateFieldCfg s e a
dateFormatDDMMYYYY = def {
  _dfcDateFormat = Just FormatDDMMYYYY
}

-- | Date format MM/DD/YYYY, using the appropriate delimiter.
dateFormatMMDDYYYY :: DateFieldCfg s e a
dateFormatMMDDYYYY = def {
  _dfcDateFormat = Just FormatMMDDYYYY
}

-- | Date format YYYY/MM/DD, using the appropriate delimiter.
dateFormatYYYYMMDD :: DateFieldCfg s e a
dateFormatYYYYMMDD = def {
  _dfcDateFormat = Just FormatYYYYMMDD
}

-- | Creates a date field using the given lens.
dateField
  :: (FormattableDate a, WidgetEvent e)
  => ALens' s a -> WidgetNode s e
dateField field = dateField_ field def

-- | Creates a date field using the given lens. Accepts config.
dateField_
  :: (FormattableDate a, WidgetEvent e)
  => ALens' s a
  -> [DateFieldCfg s e a]
  -> WidgetNode s e
dateField_ field configs = widget where
  widget = dateFieldD_ (WidgetLens field) configs

-- | Creates a date field using the given value and 'onChange' event handler.
dateFieldV
  :: (FormattableDate a, WidgetEvent e)
  => a -> (a -> e) -> WidgetNode s e
dateFieldV value handler = dateFieldV_ value handler def

-- | Creates a date field using the given value and 'onChange' event handler.
--   Accepts config.
dateFieldV_
  :: (FormattableDate a, WidgetEvent e)
  => a
  -> (a -> e)
  -> [DateFieldCfg s e a]
  -> WidgetNode s e
dateFieldV_ value handler configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = dateFieldD_ widgetData newConfigs

-- | Creates a date field providing a 'WidgetData' instance and config.
dateFieldD_
  :: (FormattableDate a, WidgetEvent e)
  => WidgetData s a
  -> [DateFieldCfg s e a]
  -> WidgetNode s e
dateFieldD_ widgetData configs = newNode where
  config = mconcat configs
  format = fromMaybe defaultDateFormat (_dfcDateFormat config)
  delim = fromMaybe defaultDateDelim (_dfcDateDelim config)
  minVal = _dfcMinValue config
  maxVal = _dfcMaxValue config
  readOnly = fromMaybe False (_dfcReadOnly config)

  initialValue
    | isJust minVal = fromJust minVal
    | isJust maxVal = fromJust maxVal
    | otherwise = dateFromDay (fromGregorian 1970 1 1)

  acceptText = dateAcceptText format delim minVal maxVal
  acceptInput text = acceptText text ^. _1
  validInput text = acceptText text ^. _2
  fromText text = acceptText text ^. _3
  toText = dateToText format delim

  inputConfig = InputFieldCfg {
    _ifcPlaceholder = Nothing,
    _ifcInitialValue = initialValue,
    _ifcValue = widgetData,
    _ifcValid = _dfcValid config,
    _ifcValidV = _dfcValidV config,
    _ifcFromText = fromText,
    _ifcToText = toText,
    _ifcAcceptInput = acceptInput,
    _ifcIsValidInput = validInput,
    _ifcDefCursorEnd = True,
    _ifcDefWidth = 160,
    _ifcCaretWidth = _dfcCaretWidth config,
    _ifcCaretMs = _dfcCaretMs config,
    _ifcDisplayChar = Nothing,
    _ifcResizeOnChange = fromMaybe False (_dfcResizeOnChange config),
    _ifcSelectOnFocus = fromMaybe True (_dfcSelectOnFocus config),
    _ifcReadOnly = readOnly,
    _ifcStyle = Just L.dateFieldStyle,
    _ifcWheelHandler = if readOnly then Nothing else Just (handleWheel config),
    _ifcDragHandler =  if readOnly then Nothing else Just (handleDrag config),
    _ifcDragCursor = Just CursorSizeV,
    _ifcOnFocusReq = _dfcOnFocusReq config,
    _ifcOnBlurReq = _dfcOnBlurReq config,
    _ifcOnChangeReq = _dfcOnChangeReq config
  }
  wtype = WidgetType ("dateField-" <> showt (typeOf initialValue))
  newNode = inputField_ wtype inputConfig

handleWheel
  :: FormattableDate a
  => DateFieldCfg s e a
  -> InputFieldState a
  -> Point
  -> Point
  -> WheelDirection
  -> (Text, Int, Maybe Int)
handleWheel config state point move dir = result where
  Point _ dy = move
  sign = if dir == WheelNormal then 1 else -1
  curValue = _ifsCurrValue state
  wheelRate = fromMaybe 1 (_dfcWheelRate config)
  result = handleMove config state wheelRate curValue (dy * sign)

handleDrag
  :: FormattableDate a
  => DateFieldCfg s e a
  -> InputFieldState a
  -> Point
  -> Point
  -> (Text, Int, Maybe Int)
handleDrag config state clickPos currPos = result where
  Point _ dy = subPoint clickPos currPos
  selValue = _ifsDragSelValue state
  dragRate = fromMaybe 1 (_dfcDragRate config)
  result = handleMove config state dragRate selValue dy

handleMove
  :: FormattableDate a
  => DateFieldCfg s e a
  -> InputFieldState a
  -> Double
  -> a
  -> Double
  -> (Text, Int, Maybe Int)
handleMove config state rate value dy = result where
  format = fromMaybe defaultDateFormat (_dfcDateFormat config)
  delim = fromMaybe defaultDateDelim (_dfcDateDelim config)
  minVal = _dfcMinValue config
  maxVal = _dfcMaxValue config

  acceptText = dateAcceptText format delim minVal maxVal
  fromText text = acceptText text ^. _3
  toText = dateToText format delim

  (valid, mParsedVal, parsedVal) = case dateToDay value of
    Just val -> (True, mParsedVal, parsedVal) where
      tmpValue = addDays (round (dy * rate)) val
      mParsedVal = fromText (toText (dateFromDay tmpValue))
      parsedVal = fromJust mParsedVal
    Nothing -> (False, Nothing, undefined)
  newVal
    | isJust mParsedVal = parsedVal
    | valid && dy > 0 && isJust maxVal = fromJust maxVal
    | valid && dy < 0 && isJust minVal = fromJust minVal
    | otherwise = _ifsCurrValue state

  newText = toText newVal
  newPos = _ifsCursorPos state
  newSel = _ifsSelStart state
  result = (newText, newPos, newSel)

dateFromTextSimple
  :: (DayConverter a, FormattableDate a)
  => DateFieldFormat
  -> Char
  -> Text
  -> Maybe a
dateFromTextSimple format delim text = newDate where
  compParser = A.char delim *> A.decimal
  dateParser = (,,) <$> A.decimal <*> compParser <*> compParser
  tmpDate = case A.parseOnly dateParser text of
    Left _ -> Nothing
    Right (n1, n2, n3)
      | format == FormatDDMMYYYY -> fromGregorianValid (fromIntegral n3) n2 n1
      | format == FormatMMDDYYYY -> fromGregorianValid (fromIntegral n3) n1 n2
      | otherwise -> fromGregorianValid (fromIntegral n1) n2 n3
  newDate = tmpDate >>= dateFromDay

dateToTextSimple :: FormattableDate a => DateFieldFormat -> Char -> a -> Text
dateToTextSimple format delim val = result where
  converted = dateToDay val
  (year, month, day) = toGregorian (fromJust converted)
  sep = T.singleton delim
  padd num
    | num < 10 = "0" <> T.pack (show num)
    | otherwise = T.pack (show num)
  tday = padd day
  tmonth = padd month
  tyear = T.pack (show year)
  result
    | isNothing converted = ""
    | format == FormatDDMMYYYY = tday <> sep <> tmonth <> sep <> tyear
    | format == FormatMMDDYYYY = tmonth <> sep <> tday <> sep <> tyear
    | otherwise = tyear <> sep <> tmonth <> sep <> tday

acceptTextInput :: DateFieldFormat -> Char -> Text -> Bool
acceptTextInput format delim text = isRight (A.parseOnly parser text) where
  numP = A.digit *> ""
  delimP = A.char delim *> ""
  dayP = P.upto 2 numP
  monthP = P.upto 2 numP
  yearP = P.upto 4 numP
  withDelim parser = A.option "" (delimP *> parser)
  parsers
    | format == FormatDDMMYYYY = [dayP, withDelim monthP, withDelim yearP]
    | format == FormatMMDDYYYY = [monthP, withDelim dayP, withDelim yearP]
    | otherwise = [yearP, withDelim monthP, withDelim dayP]
  parser = P.join parsers <* A.endOfInput

dateInBounds :: (Ord a) => Maybe a -> Maybe a -> a -> Bool
dateInBounds Nothing Nothing _ = True
dateInBounds (Just minVal) Nothing val = val >= minVal
dateInBounds Nothing (Just maxVal) val = val <= maxVal
dateInBounds (Just minVal) (Just maxVal) val = val >= minVal && val <= maxVal
