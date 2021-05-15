{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Monomer.Widgets.Singles.TimeField (
  timeField,
  timeField_,
  timeFieldV,
  timeFieldV_,
  timeFormatHHMM,
  timeFormatHHMMSS
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

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event.Types
import Monomer.Widgets.Singles.Base.InputField

import qualified Monomer.Lens as L
import qualified Monomer.Widgets.Util.Parser as P

data TimeFormat
  = FormatHHMM
  | FormatHHMMSS
  deriving (Eq, Show)

defaultTimeFormat :: TimeFormat
defaultTimeFormat = FormatHHMM

defaultTimeDelim :: Char
defaultTimeDelim = ':'

class (Eq a, Ord a, Show a, Typeable a) => TimeOfDayConverter a where
  convertFromTimeOfDay :: TimeOfDay -> a
  convertToTimeOfDay :: a -> Maybe TimeOfDay

instance TimeOfDayConverter TimeOfDay where
  convertFromTimeOfDay = id
  convertToTimeOfDay = Just

class TimeTextConverter a where
  timeAcceptText :: TimeFormat -> Maybe a -> Maybe a -> Text -> (Bool, Bool, Maybe a)
  timeFromText :: TimeFormat -> Text -> Maybe a
  timeToText :: TimeFormat -> a -> Text
  timeFromTimeOfDay' :: TimeOfDay -> a
  timeToTimeOfDay' :: a -> Maybe TimeOfDay

instance {-# OVERLAPPABLE #-} TimeOfDayConverter a => TimeTextConverter a where
  timeAcceptText format minVal maxVal text = result where
    accept = acceptTextInput format text
    parsed = timeFromText format text
    isValid = isJust parsed && timeInBounds minVal maxVal (fromJust parsed)
    fromText
      | isValid = parsed
      | otherwise = Nothing
    result = (accept, isValid, fromText)
  timeFromText = timeFromTextSimple
  timeToText = timeToTextSimple
  timeFromTimeOfDay' = convertFromTimeOfDay
  timeToTimeOfDay' = convertToTimeOfDay

instance (TimeOfDayConverter a, TimeTextConverter a) => TimeTextConverter (Maybe a) where
  timeAcceptText format minVal maxVal text
    | T.strip text == "" = (True, True, Just Nothing)
    | otherwise = (accept, isValid, result) where
      resp = timeAcceptText format (join minVal) (join maxVal) text
      (accept, isValid, tmpResult) = resp
      result
        | isJust tmpResult = Just tmpResult
        | otherwise = Nothing
  timeFromText format = Just . timeFromText format
  timeToText format Nothing = ""
  timeToText format (Just value) = timeToText format value
  timeFromTimeOfDay' = Just . timeFromTimeOfDay'
  timeToTimeOfDay' Nothing = Nothing
  timeToTimeOfDay' (Just value) = timeToTimeOfDay' value

type FormattableTime a
  = (Eq a, Ord a, Show a, TimeTextConverter a, Typeable a)

data TimeFieldCfg s e a = TimeFieldCfg {
  _tfcValid :: Maybe (WidgetData s Bool),
  _tfcTimeFormat :: Maybe TimeFormat,
  _tfcMinValue :: Maybe a,
  _tfcMaxValue :: Maybe a,
  _tfcWheelRate :: Maybe Double,
  _tfcDragRate :: Maybe Double,
  _tfcResizeOnChange :: Maybe Bool,
  _tfcSelectOnFocus :: Maybe Bool,
  _tfcOnFocus :: [Path -> e],
  _tfcOnFocusReq :: [WidgetRequest s e],
  _tfcOnBlur :: [Path -> e],
  _tfcOnBlurReq :: [WidgetRequest s e],
  _tfcOnChange :: [a -> e],
  _tfcOnChangeReq :: [a -> WidgetRequest s e]
}

instance Default (TimeFieldCfg s e a) where
  def = TimeFieldCfg {
    _tfcValid = Nothing,
    _tfcTimeFormat = Nothing,
    _tfcMinValue = Nothing,
    _tfcMaxValue = Nothing,
    _tfcWheelRate = Nothing,
    _tfcDragRate = Nothing,
    _tfcResizeOnChange = Nothing,
    _tfcSelectOnFocus = Nothing,
    _tfcOnFocus = [],
    _tfcOnFocusReq = [],
    _tfcOnBlur = [],
    _tfcOnBlurReq = [],
    _tfcOnChange = [],
    _tfcOnChangeReq = []
  }

instance Semigroup (TimeFieldCfg s e a) where
  (<>) t1 t2 = TimeFieldCfg {
    _tfcValid = _tfcValid t2 <|> _tfcValid t1,
    _tfcTimeFormat = _tfcTimeFormat t2 <|> _tfcTimeFormat t1,
    _tfcMinValue = _tfcMinValue t2 <|> _tfcMinValue t1,
    _tfcMaxValue = _tfcMaxValue t2 <|> _tfcMaxValue t1,
    _tfcWheelRate = _tfcWheelRate t2 <|> _tfcWheelRate t1,
    _tfcDragRate = _tfcDragRate t2 <|> _tfcDragRate t1,
    _tfcResizeOnChange = _tfcResizeOnChange t2 <|> _tfcResizeOnChange t1,
    _tfcSelectOnFocus = _tfcSelectOnFocus t2 <|> _tfcSelectOnFocus t1,
    _tfcOnFocus = _tfcOnFocus t1 <> _tfcOnFocus t2,
    _tfcOnFocusReq = _tfcOnFocusReq t1 <> _tfcOnFocusReq t2,
    _tfcOnBlur = _tfcOnBlur t1 <> _tfcOnBlur t2,
    _tfcOnBlurReq = _tfcOnBlurReq t1 <> _tfcOnBlurReq t2,
    _tfcOnChange = _tfcOnChange t1 <> _tfcOnChange t2,
    _tfcOnChangeReq = _tfcOnChangeReq t1 <> _tfcOnChangeReq t2
  }

instance Monoid (TimeFieldCfg s e a) where
  mempty = def

instance CmbValidInput (TimeFieldCfg s e a) s where
  validInput field = def {
    _tfcValid = Just (WidgetLens field)
  }

instance CmbResizeOnChange (TimeFieldCfg s e a) where
  resizeOnChange_ resize = def {
    _tfcResizeOnChange = Just resize
  }

instance CmbSelectOnFocus (TimeFieldCfg s e a) where
  selectOnFocus_ sel = def {
    _tfcSelectOnFocus = Just sel
  }

instance FormattableTime a => CmbMinValue (TimeFieldCfg s e a) a where
  minValue len = def {
    _tfcMinValue = Just len
  }

instance FormattableTime a => CmbMaxValue (TimeFieldCfg s e a) a where
  maxValue len = def {
    _tfcMaxValue = Just len
  }

instance CmbWheelRate (TimeFieldCfg s e a) Double where
  wheelRate rate = def {
    _tfcWheelRate = Just rate
  }

instance CmbDragRate (TimeFieldCfg s e a) Double where
  dragRate rate = def {
    _tfcDragRate = Just rate
  }

instance CmbOnFocus (TimeFieldCfg s e a) e Path where
  onFocus fn = def {
    _tfcOnFocus = [fn]
  }

instance CmbOnFocusReq (TimeFieldCfg s e a) s e where
  onFocusReq req = def {
    _tfcOnFocusReq = [req]
  }

instance CmbOnBlur (TimeFieldCfg s e a) e Path where
  onBlur fn = def {
    _tfcOnBlur = [fn]
  }

instance CmbOnBlurReq (TimeFieldCfg s e a) s e where
  onBlurReq req = def {
    _tfcOnBlurReq = [req]
  }

instance CmbOnChange (TimeFieldCfg s e a) a e where
  onChange fn = def {
    _tfcOnChange = [fn]
  }

instance CmbOnChangeReq (TimeFieldCfg s e a) s e a where
  onChangeReq req = def {
    _tfcOnChangeReq = [req]
  }

timeFormatHHMM :: TimeFieldCfg s e a
timeFormatHHMM = def {
  _tfcTimeFormat = Just FormatHHMM
}

timeFormatHHMMSS :: TimeFieldCfg s e a
timeFormatHHMMSS = def {
  _tfcTimeFormat = Just FormatHHMMSS
}

timeField
  :: (FormattableTime a, WidgetEvent e)
  => ALens' s a -> WidgetNode s e
timeField field = timeField_ field def

timeField_
  :: (FormattableTime a, WidgetEvent e)
  => ALens' s a
  -> [TimeFieldCfg s e a]
  -> WidgetNode s e
timeField_ field configs = widget where
  widget = timeFieldD_ (WidgetLens field) configs

timeFieldV
  :: (FormattableTime a, WidgetEvent e)
  => a -> (a -> e) -> WidgetNode s e
timeFieldV value handler = timeFieldV_ value handler def

timeFieldV_
  :: (FormattableTime a, WidgetEvent e)
  => a
  -> (a -> e)
  -> [TimeFieldCfg s e a]
  -> WidgetNode s e
timeFieldV_ value handler configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = timeFieldD_ widgetData newConfigs

timeFieldD_
  :: (FormattableTime a, WidgetEvent e)
  => WidgetData s a
  -> [TimeFieldCfg s e a]
  -> WidgetNode s e
timeFieldD_ widgetData configs = newNode where
  config = mconcat configs
  format = fromMaybe defaultTimeFormat (_tfcTimeFormat config)
  minVal = _tfcMinValue config
  maxVal = _tfcMaxValue config
  initialValue
    | isJust minVal = fromJust minVal
    | isJust maxVal = fromJust maxVal
    | otherwise = timeFromTimeOfDay' midnight
  acceptText = timeAcceptText format minVal maxVal
  acceptInput text = acceptText text ^. _1
  validInput text = acceptText text ^. _2
  fromText text = acceptText text ^. _3
  toText = timeToText format
  inputConfig = InputFieldCfg {
    _ifcInitialValue = initialValue,
    _ifcValue = widgetData,
    _ifcValid = _tfcValid config,
    _ifcFromText = fromText,
    _ifcToText = toText,
    _ifcAcceptInput = acceptInput,
    _ifcIsValidInput = validInput,
    _ifcDefCursorEnd = True,
    _ifcDefWidth = 160,
    _ifcResizeOnChange = fromMaybe False (_tfcResizeOnChange config),
    _ifcSelectOnFocus = fromMaybe True (_tfcSelectOnFocus config),
    _ifcSelectDragOnlyFocused = True,
    _ifcStyle = Just L.inputNumericStyle,
    _ifcWheelHandler = Just (handleWheel config),
    _ifcDragHandler = Just (handleDrag config),
    _ifcDragCursor = Just CursorSizeV,
    _ifcOnFocus = _tfcOnFocus config,
    _ifcOnFocusReq = _tfcOnFocusReq config,
    _ifcOnBlur = _tfcOnBlur config,
    _ifcOnBlurReq = _tfcOnBlurReq config,
    _ifcOnChange = _tfcOnChange config,
    _ifcOnChangeReq = _tfcOnChangeReq config
  }
  newNode = inputField_ "timeField" inputConfig

handleWheel
  :: FormattableTime a
  => TimeFieldCfg s e a
  -> InputFieldState a
  -> Point
  -> Point
  -> WheelDirection
  -> (Text, Int, Maybe Int)
handleWheel config state point move dir = result where
  Point _ dy = move
  sign = if dir == WheelNormal then 1 else -1
  curValue = _ifsCurrValue state
  wheelRate = fromMaybe 1 (_tfcWheelRate config)
  result = handleMove config state wheelRate curValue (dy * sign)

handleDrag
  :: FormattableTime a
  => TimeFieldCfg s e a
  -> InputFieldState a
  -> Point
  -> Point
  -> (Text, Int, Maybe Int)
handleDrag config state clickPos currPos = result where
  Point _ dy = subPoint clickPos currPos
  selValue = _ifsDragSelValue state
  dragRate = fromMaybe 1 (_tfcDragRate config)
  result = handleMove config state dragRate selValue dy

handleMove
  :: FormattableTime a
  => TimeFieldCfg s e a
  -> InputFieldState a
  -> Double
  -> a
  -> Double
  -> (Text, Int, Maybe Int)
handleMove config state rate value dy = result where
  format = fromMaybe defaultTimeFormat (_tfcTimeFormat config)
  minVal = _tfcMinValue config
  maxVal = _tfcMaxValue config
  acceptText = timeAcceptText format minVal maxVal
  fromText text = acceptText text ^. _3
  toText = timeToText format
  (valid, mParsedVal, parsedVal) = case timeToTimeOfDay' value of
    Just val -> (True, mParsedVal, parsedVal) where
      tmpValue = addMinutes (round (dy * rate)) val
      mParsedVal = fromText (toText (timeFromTimeOfDay' tmpValue))
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

timeFromTextSimple
  :: (TimeOfDayConverter a, FormattableTime a)
  => TimeFormat
  -> Text
  -> Maybe a
timeFromTextSimple format text = newTime where
  compParser = A.char ':' *> A.decimal
  timeParser
    | format == FormatHHMM = (,,) <$> A.decimal <*> compParser <*> pure 0
    | otherwise = (,,) <$> A.decimal <*> compParser <*> compParser
  tmpTime = case A.parseOnly timeParser text of
    Left _ -> Nothing
    Right (n1, n2, n3)
      | format == FormatHHMM -> makeTimeOfDayValid n1 n2 0
      | otherwise -> makeTimeOfDayValid n1 n2 (fromIntegral n3)
  newTime = tmpTime >>= timeFromTimeOfDay'

timeToTextSimple :: FormattableTime a => TimeFormat -> a -> Text
timeToTextSimple format val = result where
  sep = T.singleton defaultTimeDelim
  converted = timeToTimeOfDay' val
  TimeOfDay hh mm ss = fromJust converted
  padd num
    | num < 10 = "0" <> T.pack (show num)
    | otherwise = T.pack (show num)
  thh = padd hh
  tmm = padd mm
  tss = padd (round ss)
  result
    | isNothing converted = ""
    | format == FormatHHMM = thh <> sep <> tmm
    | otherwise = thh <> sep <> tmm <> sep <> tss

acceptTextInput :: TimeFormat -> Text -> Bool
acceptTextInput format text = isRight (A.parseOnly parser text) where
  numP = A.digit *> ""
  delimP = A.char defaultTimeDelim *> ""
  hhP = P.upto 2 numP
  mmP = P.upto 2 numP
  ssP = P.upto 2 numP
  withDelim parser = A.option "" (delimP *> parser)
  parsers
    | format == FormatHHMM = [hhP, withDelim mmP]
    | otherwise = [hhP, withDelim mmP, withDelim ssP]
  parser = P.join parsers <* A.endOfInput

timeInBounds :: (Ord a) => Maybe a -> Maybe a -> a -> Bool
timeInBounds Nothing Nothing _ = True
timeInBounds (Just minVal) Nothing val = val >= minVal
timeInBounds Nothing (Just maxVal) val = val <= maxVal
timeInBounds (Just minVal) (Just maxVal) val = val >= minVal && val <= maxVal

addMinutes :: Int -> TimeOfDay -> TimeOfDay
addMinutes mins time = newTime where
  baseDate = fromGregorian 2000 1 1
  baseTime = timeOfDayToTime time
  baseUTC = UTCTime baseDate baseTime
  UTCTime newDate diff = addUTCTime (fromIntegral mins * 60) baseUTC
  newTime
    | newDate > baseDate = TimeOfDay 23 59 59
    | newDate < baseDate = TimeOfDay 0 0 0
    | otherwise = timeToTimeOfDay diff
