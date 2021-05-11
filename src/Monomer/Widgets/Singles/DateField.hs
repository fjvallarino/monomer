{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Monomer.Widgets.Singles.DateField (
  dateField,
  dateField_,
  dateFieldV,
  dateFieldV_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens')
import Data.Default
import Data.Either
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Data.Typeable (Typeable, typeOf)

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event.Types
import Monomer.Widgets.Singles.Base.InputField
import Monomer.Widgets.Util.Parser

import qualified Monomer.Lens as L

class DayConverter a where
  convertFromDay :: Day -> a
  convertToDay :: a -> Maybe Day

instance DayConverter Day where
  convertFromDay = id
  convertToDay = Just

instance DayConverter a => DayConverter (Maybe a) where
  convertFromDay = Just . convertFromDay
  convertToDay val = val >>= convertToDay

type FormattableDate a
  = (Eq a, Ord a, Show a, DayConverter a, Typeable a)

data DateldCfg s e a = DateldCfg {
  _dfcValid :: Maybe (WidgetData s Bool),
  _dfcMinValue :: Maybe a,
  _dfcMaxValue :: Maybe a,
  _dfcWheelRate :: Maybe Double,
  _dfcDragRate :: Maybe Double,
  _dfcResizeOnChange :: Maybe Bool,
  _dfcSelectOnFocus :: Maybe Bool,
  _dfcOnFocus :: [Path -> e],
  _dfcOnFocusReq :: [WidgetRequest s e],
  _dfcOnBlur :: [Path -> e],
  _dfcOnBlurReq :: [WidgetRequest s e],
  _dfcOnChange :: [a -> e],
  _dfcOnChangeReq :: [a -> WidgetRequest s e]
}

instance Default (DateldCfg s e a) where
  def = DateldCfg {
    _dfcValid = Nothing,
    _dfcMinValue = Nothing,
    _dfcMaxValue = Nothing,
    _dfcWheelRate = Nothing,
    _dfcDragRate = Nothing,
    _dfcResizeOnChange = Nothing,
    _dfcSelectOnFocus = Nothing,
    _dfcOnFocus = [],
    _dfcOnFocusReq = [],
    _dfcOnBlur = [],
    _dfcOnBlurReq = [],
    _dfcOnChange = [],
    _dfcOnChangeReq = []
  }

instance Semigroup (DateldCfg s e a) where
  (<>) t1 t2 = DateldCfg {
    _dfcValid = _dfcValid t2 <|> _dfcValid t1,
    _dfcMinValue = _dfcMinValue t2 <|> _dfcMinValue t1,
    _dfcMaxValue = _dfcMaxValue t2 <|> _dfcMaxValue t1,
    _dfcWheelRate = _dfcWheelRate t2 <|> _dfcWheelRate t1,
    _dfcDragRate = _dfcDragRate t2 <|> _dfcDragRate t1,
    _dfcResizeOnChange = _dfcResizeOnChange t2 <|> _dfcResizeOnChange t1,
    _dfcSelectOnFocus = _dfcSelectOnFocus t2 <|> _dfcSelectOnFocus t1,
    _dfcOnFocus = _dfcOnFocus t1 <> _dfcOnFocus t2,
    _dfcOnFocusReq = _dfcOnFocusReq t1 <> _dfcOnFocusReq t2,
    _dfcOnBlur = _dfcOnBlur t1 <> _dfcOnBlur t2,
    _dfcOnBlurReq = _dfcOnBlurReq t1 <> _dfcOnBlurReq t2,
    _dfcOnChange = _dfcOnChange t1 <> _dfcOnChange t2,
    _dfcOnChangeReq = _dfcOnChangeReq t1 <> _dfcOnChangeReq t2
  }

instance Monoid (DateldCfg s e a) where
  mempty = def

instance CmbValidInput (DateldCfg s e a) s where
  validInput field = def {
    _dfcValid = Just (WidgetLens field)
  }

instance CmbResizeOnChange (DateldCfg s e a) where
  resizeOnChange_ resize = def {
    _dfcResizeOnChange = Just resize
  }

instance CmbSelectOnFocus (DateldCfg s e a) where
  selectOnFocus_ sel = def {
    _dfcSelectOnFocus = Just sel
  }

instance FormattableDate a => CmbMinValue (DateldCfg s e a) a where
  minValue len = def {
    _dfcMinValue = Just len
  }

instance FormattableDate a => CmbMaxValue (DateldCfg s e a) a where
  maxValue len = def {
    _dfcMaxValue = Just len
  }

instance CmbWheelRate (DateldCfg s e a) Double where
  wheelRate rate = def {
    _dfcWheelRate = Just rate
  }

instance CmbDragRate (DateldCfg s e a) Double where
  dragRate rate = def {
    _dfcDragRate = Just rate
  }

instance CmbOnFocus (DateldCfg s e a) e Path where
  onFocus fn = def {
    _dfcOnFocus = [fn]
  }

instance CmbOnFocusReq (DateldCfg s e a) s e where
  onFocusReq req = def {
    _dfcOnFocusReq = [req]
  }

instance CmbOnBlur (DateldCfg s e a) e Path where
  onBlur fn = def {
    _dfcOnBlur = [fn]
  }

instance CmbOnBlurReq (DateldCfg s e a) s e where
  onBlurReq req = def {
    _dfcOnBlurReq = [req]
  }

instance CmbOnChange (DateldCfg s e a) a e where
  onChange fn = def {
    _dfcOnChange = [fn]
  }

instance CmbOnChangeReq (DateldCfg s e a) s e a where
  onChangeReq req = def {
    _dfcOnChangeReq = [req]
  }

dateField
  :: (FormattableDate a, WidgetEvent e)
  => ALens' s a -> WidgetNode s e
dateField field = dateField_ field def

dateField_
  :: (FormattableDate a, WidgetEvent e)
  => ALens' s a
  -> [DateldCfg s e a]
  -> WidgetNode s e
dateField_ field configs = widget where
  widget = dateFieldD_ (WidgetLens field) configs

dateFieldV
  :: (FormattableDate a, WidgetEvent e)
  => a -> (a -> e) -> WidgetNode s e
dateFieldV value handler = dateFieldV_ value handler def

dateFieldV_
  :: (FormattableDate a, WidgetEvent e)
  => a
  -> (a -> e)
  -> [DateldCfg s e a]
  -> WidgetNode s e
dateFieldV_ value handler configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = dateFieldD_ widgetData newConfigs

dateFieldD_
  :: (FormattableDate a, WidgetEvent e)
  => WidgetData s a
  -> [DateldCfg s e a]
  -> WidgetNode s e
dateFieldD_ widgetData configs = newNode where
  config = mconcat configs
  minVal = _dfcMinValue config
  maxVal = _dfcMaxValue config
  initialValue
    | isJust minVal = fromJust minVal
    | isJust maxVal = fromJust maxVal
    | otherwise = convertFromDay (fromGregorian 1970 1 1)
  fromText = dateFromText minVal maxVal
  toText = dateToText
  inputConfig = InputFieldCfg {
    _ifcInitialValue = initialValue,
    _ifcValue = widgetData,
    _ifcValid = _dfcValid config,
    _ifcFromText = fromText,
    _ifcToText = toText,
    _ifcAcceptInput = acceptTextInput,
    _ifcDefCursorEnd = True,
    _ifcDefWidth = 160,
    _ifcResizeOnChange = fromMaybe False (_dfcResizeOnChange config),
    _ifcSelectOnFocus = fromMaybe True (_dfcSelectOnFocus config),
    _ifcSelectDragOnlyFocused = True,
    _ifcStyle = Just L.inputNumericStyle,
    _ifcWheelHandler = Just (handleWheel config),
    _ifcDragHandler = Just (handleDrag config),
    _ifcDragCursor = Just CursorSizeV,
    _ifcOnFocus = _dfcOnFocus config,
    _ifcOnFocusReq = _dfcOnFocusReq config,
    _ifcOnBlur = _dfcOnBlur config,
    _ifcOnBlurReq = _dfcOnBlurReq config,
    _ifcOnChange = _dfcOnChange config,
    _ifcOnChangeReq = _dfcOnChangeReq config
  }
  newNode = inputField_ "dateField" inputConfig

handleWheel
  :: FormattableDate a
  => DateldCfg s e a
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
  => DateldCfg s e a
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
  => DateldCfg s e a
  -> InputFieldState a
  -> Double
  -> a
  -> Double
  -> (Text, Int, Maybe Int)
handleMove config state rate value dy = result where
  minVal = _dfcMinValue config
  maxVal = _dfcMaxValue config
  fromText = dateFromText minVal maxVal
  toText = dateToText
  (valid, mParsedVal, parsedVal) = case convertToDay value of
    Just val -> (True, mParsedVal, parsedVal) where
      tmpValue = addDays (round (dy * rate)) val
      mParsedVal = fromText (toText (convertFromDay tmpValue))
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

dateFromText :: FormattableDate a => Maybe a -> Maybe a -> Text -> Maybe a
dateFromText minVal maxVal text = newDate where
  compParser = A.char '/' *> A.decimal
  dateParser = (,,) <$> A.decimal <*> compParser <*> compParser
  tmpDate = case A.parseOnly dateParser text of
    Left _ -> Nothing
    Right (day, month, year) -> fromGregorianValid year (fromIntegral month) day
  validDate date
    | dateInBounds minVal maxVal date = Just date
    | otherwise = Nothing
  newDate = tmpDate >>= validDate . convertFromDay

dateToText :: FormattableDate a => a -> Text
dateToText val = result where
  converted = convertToDay val
  (year, month, day) = toGregorian (fromJust converted)
  padd num
    | num < 10 = "0" <> T.pack (show num)
    | otherwise = T.pack (show num)
  tday = padd day
  tmonth = padd month
  tyear = T.pack (show year)
  result
    | isJust converted = tday <> "/" <> tmonth <> "/" <> tyear
    | otherwise = ""

acceptTextInput :: Text -> Bool
acceptTextInput text = isRight (A.parseOnly parser text) where
  barP = A.char '/' *> ""
  numP = A.digit *> ""
  dayP = upto 2 numP
  monthP = A.option "" (barP *> upto 2 numP)
  yearP = A.option "" (barP *> upto 4 numP)
  parser = join [dayP, monthP, yearP] <* A.endOfInput

dateInBounds :: (Ord a) => Maybe a -> Maybe a -> a -> Bool
dateInBounds Nothing Nothing _ = True
dateInBounds (Just minVal) Nothing val = val >= minVal
dateInBounds Nothing (Just maxVal) val = val <= maxVal
dateInBounds (Just minVal) (Just maxVal) val = val >= minVal && val <= maxVal
