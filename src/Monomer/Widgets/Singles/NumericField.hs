{-|
Module      : Monomer.Widgets.Singles.NumericField
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Input field for numeric types.

Supports instances of the 'FromFractional' typeclass. Several basic types are
implemented, both for integer and floating point types.

Handles mouse wheel and vertical drag to increase/decrease the number.

Configs:

- validInput: field indicating if the current input is valid. Useful to show
warnings in the UI, or disable buttons if needed.
- resizeOnChange: Whether input causes ResizeWidgets requests.
- selectOnFocus: Whether all input should be selected when focus is received.
- minValue: Minimum valid number.
- maxValue: Maximum valid number.
- wheelRate: The rate at which wheel movement affects the number.
- dragRate: The rate at which drag movement affects the number.
- onFocus: event to raise when focus is received.
- onFocusReq: WidgetRequest to generate when focus is received.
- onBlur: event to raise when focus is lost.
- onBlurReq: WidgetRequest to generate when focus is lost.
- onChange: event to raise when the value changes.
- onChangeReq: WidgetRequest to generate when the value changes.
- decimals: the maximum number of digits after the decimal separator. Defaults
to zero for integers and two for floating point types.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Monomer.Widgets.Singles.NumericField (
  numericField,
  numericField_,
  numericFieldV,
  numericFieldV_
) where

import Control.Applicative ((<|>))
import Control.Lens ((^.), ALens', _1, _2, _3)
import Control.Monad (join)
import Data.Char
import Data.Default
import Data.Either
import Data.List (isPrefixOf)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Read (signed, rational)
import Data.Typeable (Typeable, typeOf)

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Formatting as F

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event.Types
import Monomer.Widgets.Singles.Base.InputField

import qualified Monomer.Lens as L
import qualified Monomer.Widgets.Util.Parser as P

class NumericTextConverter a where
  numericAcceptText :: Maybe a -> Maybe a -> Int -> Text -> (Bool, Bool, Maybe a)
  numericFromText :: Text -> Maybe a
  numericToText :: Int -> a -> Text
  numericToFractional :: Fractional b => a -> Maybe b
  numericFromFractional :: (Real b, Fractional b) => b -> a

instance {-# OVERLAPPABLE #-} FromFractional a => NumericTextConverter a where
  numericAcceptText minVal maxVal decimals text = result where
    accept = acceptNumberInput decimals text
    parsed = numericFromText text
    isValid = isJust parsed && numberInBounds minVal maxVal (fromJust parsed)
    fromText
      | isValid = parsed
      | otherwise = Nothing
    result = (accept, isValid, fromText)
  numericFromText text = case signed rational text of
    Right (frac :: Rational, _) -> Just (fromFractional frac)
    _ -> Nothing
  numericToText decimals value = F.sformat (F.fixed decimals) value
  numericToFractional = Just . realToFrac
  numericFromFractional = fromFractional

instance (FromFractional a, NumericTextConverter a) => NumericTextConverter (Maybe a) where
  numericAcceptText minVal maxVal decimals text
    | T.strip text == "" = (True, True, Just Nothing)
    | otherwise = (accept, isValid, result) where
      resp = numericAcceptText (join minVal) (join maxVal) decimals text
      (accept, isValid, tmpResult) = resp
      result
        | isJust tmpResult = Just tmpResult
        | otherwise = Nothing
  numericFromText = Just . numericFromText
  numericToText _ Nothing = ""
  numericToText decimals (Just value) = numericToText decimals value
  numericToFractional Nothing = Nothing
  numericToFractional (Just value) = numericToFractional value
  numericFromFractional = Just . numericFromFractional

type FormattableNumber a
  = (Eq a, Ord a, Show a, NumericTextConverter a, Typeable a)

data NumericFieldCfg s e a = NumericFieldCfg {
  _nfcValid :: Maybe (WidgetData s Bool),
  _nfcValidV :: [Bool -> e],
  _nfcDecimals :: Maybe Int,
  _nfcMinValue :: Maybe a,
  _nfcMaxValue :: Maybe a,
  _nfcWheelRate :: Maybe Double,
  _nfcDragRate :: Maybe Double,
  _nfcResizeOnChange :: Maybe Bool,
  _nfcSelectOnFocus :: Maybe Bool,
  _nfcOnFocusReq :: [Path -> WidgetRequest s e],
  _nfcOnBlurReq :: [Path -> WidgetRequest s e],
  _nfcOnChange :: [a -> e],
  _nfcOnChangeReq :: [a -> WidgetRequest s e]
}

instance Default (NumericFieldCfg s e a) where
  def = NumericFieldCfg {
    _nfcValid = Nothing,
    _nfcValidV = [],
    _nfcDecimals = Nothing,
    _nfcMinValue = Nothing,
    _nfcMaxValue = Nothing,
    _nfcWheelRate = Nothing,
    _nfcDragRate = Nothing,
    _nfcResizeOnChange = Nothing,
    _nfcSelectOnFocus = Nothing,
    _nfcOnFocusReq = [],
    _nfcOnBlurReq = [],
    _nfcOnChange = [],
    _nfcOnChangeReq = []
  }

instance Semigroup (NumericFieldCfg s e a) where
  (<>) t1 t2 = NumericFieldCfg {
    _nfcValid = _nfcValid t2 <|> _nfcValid t1,
    _nfcValidV = _nfcValidV t1 <> _nfcValidV t2,
    _nfcDecimals = _nfcDecimals t2 <|> _nfcDecimals t1,
    _nfcMinValue = _nfcMinValue t2 <|> _nfcMinValue t1,
    _nfcMaxValue = _nfcMaxValue t2 <|> _nfcMaxValue t1,
    _nfcWheelRate = _nfcWheelRate t2 <|> _nfcWheelRate t1,
    _nfcDragRate = _nfcDragRate t2 <|> _nfcDragRate t1,
    _nfcResizeOnChange = _nfcResizeOnChange t2 <|> _nfcResizeOnChange t1,
    _nfcSelectOnFocus = _nfcSelectOnFocus t2 <|> _nfcSelectOnFocus t1,
    _nfcOnFocusReq = _nfcOnFocusReq t1 <> _nfcOnFocusReq t2,
    _nfcOnBlurReq = _nfcOnBlurReq t1 <> _nfcOnBlurReq t2,
    _nfcOnChange = _nfcOnChange t1 <> _nfcOnChange t2,
    _nfcOnChangeReq = _nfcOnChangeReq t1 <> _nfcOnChangeReq t2
  }

instance Monoid (NumericFieldCfg s e a) where
  mempty = def

instance CmbValidInput (NumericFieldCfg s e a) s where
  validInput field = def {
    _nfcValid = Just (WidgetLens field)
  }

instance CmbValidInputV (NumericFieldCfg s e a) e where
  validInputV fn = def {
    _nfcValidV = [fn]
  }

instance CmbResizeOnChange (NumericFieldCfg s e a) where
  resizeOnChange_ resize = def {
    _nfcResizeOnChange = Just resize
  }

instance CmbSelectOnFocus (NumericFieldCfg s e a) where
  selectOnFocus_ sel = def {
    _nfcSelectOnFocus = Just sel
  }

instance FormattableNumber a => CmbMinValue (NumericFieldCfg s e a) a where
  minValue len = def {
    _nfcMinValue = Just len
  }

instance FormattableNumber a => CmbMaxValue (NumericFieldCfg s e a) a where
  maxValue len = def {
    _nfcMaxValue = Just len
  }

instance CmbWheelRate (NumericFieldCfg s e a) Double where
  wheelRate rate = def {
    _nfcWheelRate = Just rate
  }

instance CmbDragRate (NumericFieldCfg s e a) Double where
  dragRate rate = def {
    _nfcDragRate = Just rate
  }

instance CmbDecimals (NumericFieldCfg s e a) where
  decimals num = def {
    _nfcDecimals = Just num
  }

instance WidgetEvent e => CmbOnFocus (NumericFieldCfg s e a) e Path where
  onFocus fn = def {
    _nfcOnFocusReq = [RaiseEvent . fn]
  }

instance CmbOnFocusReq (NumericFieldCfg s e a) s e Path where
  onFocusReq req = def {
    _nfcOnFocusReq = [req]
  }

instance WidgetEvent e => CmbOnBlur (NumericFieldCfg s e a) e Path where
  onBlur fn = def {
    _nfcOnBlurReq = [RaiseEvent . fn]
  }

instance CmbOnBlurReq (NumericFieldCfg s e a) s e Path where
  onBlurReq req = def {
    _nfcOnBlurReq = [req]
  }

instance CmbOnChange (NumericFieldCfg s e a) a e where
  onChange fn = def {
    _nfcOnChange = [fn]
  }

instance CmbOnChangeReq (NumericFieldCfg s e a) s e a where
  onChangeReq req = def {
    _nfcOnChangeReq = [req]
  }

-- | Creates a numeric field using the given lens.
numericField
  :: (FormattableNumber a, WidgetEvent e)
  => ALens' s a -> WidgetNode s e
numericField field = numericField_ field def

-- | Creates a numeric field using the given lens. Accepts config.
numericField_
  :: (FormattableNumber a, WidgetEvent e)
  => ALens' s a
  -> [NumericFieldCfg s e a]
  -> WidgetNode s e
numericField_ field configs = widget where
  widget = numericFieldD_ (WidgetLens field) configs

-- | Creates a numeric field using the given value and onChange event handler.
numericFieldV
  :: (FormattableNumber a, WidgetEvent e)
  => a -> (a -> e) -> WidgetNode s e
numericFieldV value handler = numericFieldV_ value handler def

-- | Creates a numeric field using the given value and onChange event handler.
-- | Accepts config.
numericFieldV_
  :: (FormattableNumber a, WidgetEvent e)
  => a
  -> (a -> e)
  -> [NumericFieldCfg s e a]
  -> WidgetNode s e
numericFieldV_ value handler configs = newNode where
  widgetData = WidgetValue value
  newConfigs = onChange handler : configs
  newNode = numericFieldD_ widgetData newConfigs

-- | Creates a numeric field providing a WidgetData instance and config.
numericFieldD_
  :: forall s e a . (FormattableNumber a, WidgetEvent e)
  => WidgetData s a
  -> [NumericFieldCfg s e a]
  -> WidgetNode s e
numericFieldD_ widgetData configs = newNode where
  config = mconcat configs
  minVal = _nfcMinValue config
  maxVal = _nfcMaxValue config
  initialValue
    | isJust minVal = fromJust minVal
    | isJust maxVal = fromJust maxVal
    | otherwise = numericFromFractional 0
  decimals
    | isIntegral initialValue = 0
    | otherwise = max 0 $ fromMaybe 2 (_nfcDecimals config)
  defWidth
    | isIntegral initialValue = 50
    | otherwise = 70
  acceptText = numericAcceptText minVal maxVal decimals
  acceptInput text = acceptText text ^. _1
  validInput text = acceptText text ^. _2
  fromText text = acceptText text ^. _3
  toText = numericToText decimals
  inputConfig = InputFieldCfg {
    _ifcInitialValue = initialValue,
    _ifcValue = widgetData,
    _ifcValid = _nfcValid config,
    _ifcValidV = _nfcValidV config,
    _ifcFromText = fromText,
    _ifcToText = toText,
    _ifcAcceptInput = acceptInput,
    _ifcIsValidInput = validInput,
    _ifcDefCursorEnd = False,
    _ifcDefWidth = defWidth,
    _ifcResizeOnChange = fromMaybe False (_nfcResizeOnChange config),
    _ifcSelectOnFocus = fromMaybe True (_nfcSelectOnFocus config),
    _ifcSelectDragOnlyFocused = True,
    _ifcStyle = Just L.numericFieldStyle,
    _ifcWheelHandler = Just (handleWheel config),
    _ifcDragHandler = Just (handleDrag config),
    _ifcDragCursor = Just CursorSizeV,
    _ifcOnFocusReq = _nfcOnFocusReq config,
    _ifcOnBlurReq = _nfcOnBlurReq config,
    _ifcOnChange = _nfcOnChange config,
    _ifcOnChangeReq = _nfcOnChangeReq config
  }
  newNode = inputField_ "numericField" inputConfig

handleWheel
  :: FormattableNumber a
  => NumericFieldCfg s e a
  -> InputFieldState a
  -> Point
  -> Point
  -> WheelDirection
  -> (Text, Int, Maybe Int)
handleWheel config state point move dir = result where
  Point _ dy = move
  sign = if dir == WheelNormal then 1 else -1
  curValue = _ifsCurrValue state
  wheelRate
    | isIntegral curValue = fromMaybe 1 (_nfcWheelRate config)
    | otherwise = fromMaybe 0.1 (_nfcWheelRate config)
  result = handleMove config state wheelRate curValue (dy * sign)

handleDrag
  :: FormattableNumber a
  => NumericFieldCfg s e a
  -> InputFieldState a
  -> Point
  -> Point
  -> (Text, Int, Maybe Int)
handleDrag config state clickPos currPos = result where
  Point _ dy = subPoint clickPos currPos
  selValue = _ifsDragSelValue state
  dragRate
    | isIntegral selValue = fromMaybe 1 (_nfcDragRate config)
    | otherwise = fromMaybe 0.1 (_nfcDragRate config)
  result = handleMove config state dragRate selValue dy

handleMove
  :: forall s e a . FormattableNumber a
  => NumericFieldCfg s e a
  -> InputFieldState a
  -> Double
  -> a
  -> Double
  -> (Text, Int, Maybe Int)
handleMove config state rate value dy = result where
  decimals
    | isIntegral value = 0
    | otherwise = max 0 $ fromMaybe 2 (_nfcDecimals config)
  minVal = _nfcMinValue config
  maxVal = _nfcMaxValue config
  acceptText = numericAcceptText minVal maxVal decimals
  fromText text = acceptText text ^. _3
  toText = numericToText
  (valid, mParsedVal, parsedVal) = case numericToFractional value of
    Just val -> (True, mParsedVal, parsedVal) where
      tmpValue = realToFrac val + dy * rate
      mParsedVal = fromText (toText decimals (numericFromFractional tmpValue))
      parsedVal = fromJust mParsedVal
    Nothing -> (False, Nothing, undefined)
  newVal
    | isJust mParsedVal = parsedVal
    | valid && dy > 0 && isJust maxVal = fromJust maxVal
    | valid && dy < 0 && isJust minVal = fromJust minVal
    | otherwise = _ifsCurrValue state
  newText = toText decimals newVal
  newPos = _ifsCursorPos state
  newSel = _ifsSelStart state
  result = (newText, newPos, newSel)

acceptNumberInput :: Int -> Text -> Bool
acceptNumberInput decimals text = isRight (A.parseOnly parser text) where
  sign = A.option "" (P.single '-')
  number = A.takeWhile isDigit
  digit = T.singleton <$> A.digit
  dot = P.single '.'
  dots = if decimals > 0 then 1 else 0
  rest = P.join [P.upto dots dot, P.upto decimals digit]
  parser = P.join [sign, number, A.option "" rest] <* A.endOfInput

numberInBounds :: Ord a => Maybe a -> Maybe a -> a -> Bool
numberInBounds Nothing Nothing _ = True
numberInBounds (Just minVal) Nothing val = val >= minVal
numberInBounds Nothing (Just maxVal) val = val <= maxVal
numberInBounds (Just minVal) (Just maxVal) val = val >= minVal && val <= maxVal

isIntegral :: Typeable a => a -> Bool
isIntegral val
  | "Int" `isPrefixOf` name = True
  | "Fixed" `isPrefixOf` name = True
  | "Word" `isPrefixOf` name = True
  | otherwise = False
  where
    typeName = show (typeOf val)
    name
      | "Maybe " `isPrefixOf` typeName = drop 6 typeName
      | otherwise = typeName
