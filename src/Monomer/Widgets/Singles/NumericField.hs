{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use foldr" -}

module Monomer.Widgets.Singles.NumericField (
  numericField,
  numericField_,
  numericFieldV,
  numericFieldV_
) where

import Codec.Serialise
import Control.Applicative ((<|>))
import Control.Lens (ALens')
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
import Monomer.Widgets.Singles.InputField
import Monomer.Widgets.Util

import qualified Monomer.Lens as L

type FormattableNumber a
  = (Eq a, Show a, Typeable a, Real a, FromFractional a, Serialise a)

data NumericFieldCfg s e a = NumericFieldCfg {
  _nfcValid :: Maybe (WidgetData s Bool),
  _nfcDecimals :: Maybe Int,
  _nfcMinValue :: Maybe a,
  _nfcMaxValue :: Maybe a,
  _nfcDragRate :: Maybe Double,
  _nfcResizeOnChange :: Maybe Bool,
  _nfcSelectOnFocus :: Maybe Bool,
  _nfcOnFocus :: [e],
  _nfcOnFocusReq :: [WidgetRequest s],
  _nfcOnBlur :: [e],
  _nfcOnBlurReq :: [WidgetRequest s],
  _nfcOnChange :: [a -> e],
  _nfcOnChangeReq :: [WidgetRequest s]
}

instance Default (NumericFieldCfg s e a) where
  def = NumericFieldCfg {
    _nfcValid = Nothing,
    _nfcDecimals = Nothing,
    _nfcMinValue = Nothing,
    _nfcMaxValue = Nothing,
    _nfcDragRate = Nothing,
    _nfcResizeOnChange = Nothing,
    _nfcSelectOnFocus = Nothing,
    _nfcOnFocus = [],
    _nfcOnFocusReq = [],
    _nfcOnBlur = [],
    _nfcOnBlurReq = [],
    _nfcOnChange = [],
    _nfcOnChangeReq = []
  }

instance Semigroup (NumericFieldCfg s e a) where
  (<>) t1 t2 = NumericFieldCfg {
    _nfcValid = _nfcValid t2 <|> _nfcValid t1,
    _nfcDecimals = _nfcDecimals t2 <|> _nfcDecimals t1,
    _nfcMinValue = _nfcMinValue t2 <|> _nfcMinValue t1,
    _nfcMaxValue = _nfcMaxValue t2 <|> _nfcMaxValue t1,
    _nfcDragRate = _nfcDragRate t2 <|> _nfcDragRate t1,
    _nfcResizeOnChange = _nfcResizeOnChange t2 <|> _nfcResizeOnChange t1,
    _nfcSelectOnFocus = _nfcSelectOnFocus t2 <|> _nfcSelectOnFocus t1,
    _nfcOnFocus = _nfcOnFocus t1 <> _nfcOnFocus t2,
    _nfcOnFocusReq = _nfcOnFocusReq t1 <> _nfcOnFocusReq t2,
    _nfcOnBlur = _nfcOnBlur t1 <> _nfcOnBlur t2,
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

instance CmbDragRate (NumericFieldCfg s e a) Double where
  dragRate rate = def {
    _nfcDragRate = Just rate
  }

instance CmbDecimals (NumericFieldCfg s e a) where
  decimals num = def {
    _nfcDecimals = Just num
  }

instance CmbOnFocus (NumericFieldCfg s e a) e where
  onFocus fn = def {
    _nfcOnFocus = [fn]
  }

instance CmbOnFocusReq (NumericFieldCfg s e a) s where
  onFocusReq req = def {
    _nfcOnFocusReq = [req]
  }

instance CmbOnBlur (NumericFieldCfg s e a) e where
  onBlur fn = def {
    _nfcOnBlur = [fn]
  }

instance CmbOnBlurReq (NumericFieldCfg s e a) s where
  onBlurReq req = def {
    _nfcOnBlurReq = [req]
  }

instance CmbOnChange (NumericFieldCfg s e a) a e where
  onChange fn = def {
    _nfcOnChange = [fn]
  }

instance CmbOnChangeReq (NumericFieldCfg s e a) s where
  onChangeReq req = def {
    _nfcOnChangeReq = [req]
  }

numericField
  :: (FormattableNumber a, WidgetEvent e)
  => ALens' s a -> WidgetNode s e
numericField field = numericField_ field def

numericField_
  :: (FormattableNumber a, WidgetEvent e)
  => ALens' s a
  -> [NumericFieldCfg s e a]
  -> WidgetNode s e
numericField_ field configs = widget where
  widget = numericFieldD_ (WidgetLens field) configs

numericFieldV
  :: (FormattableNumber a, WidgetEvent e)
  => a -> (a -> e) -> WidgetNode s e
numericFieldV value handler = numericFieldV_ value handler def

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

numericFieldD_
  :: (FormattableNumber a, WidgetEvent e)
  => WidgetData s a
  -> [NumericFieldCfg s e a]
  -> WidgetNode s e
numericFieldD_ widgetData configs = newNode where
  config = mconcat configs
  minVal = _nfcMinValue config
  maxVal = _nfcMaxValue config
  initialValue = fromFractional 0
  decimals
    | isIntegral initialValue = 0
    | otherwise = max 0 $ fromMaybe 2 (_nfcDecimals config)
  defWidth
    | isIntegral initialValue = 50
    | otherwise = 70
  fromText = numberFromText minVal maxVal
  toText = numberToText decimals
  inputConfig = InputFieldCfg {
    _ifcInitialValue = initialValue,
    _ifcValue = widgetData,
    _ifcValid = _nfcValid config,
    _ifcFromText = fromText,
    _ifcToText = toText,
    _ifcAcceptInput = acceptNumberInput decimals,
    _ifcDefCursorEnd = False,
    _ifcDefWidth = defWidth,
    _ifcResizeOnChange = fromMaybe False (_nfcResizeOnChange config),
    _ifcSelectOnFocus = fromMaybe True (_nfcSelectOnFocus config),
    _ifcSelectDragOnlyFocused = True,
    _ifcStyle = Just L.inputNumericStyle,
    _ifcDragHandler = Just (handleDrag config),
    _ifcDragCursor = Just CursorSizeV,
    _ifcOnFocus = _nfcOnFocus config,
    _ifcOnFocusReq = _nfcOnFocusReq config,
    _ifcOnBlur = _nfcOnBlur config,
    _ifcOnBlurReq = _nfcOnBlurReq config,
    _ifcOnChange = _nfcOnChange config,
    _ifcOnChangeReq = _nfcOnChangeReq config
  }
  newNode = inputField_ "numericField" inputConfig

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
  decimals
    | isIntegral selValue = 0
    | otherwise = max 0 $ fromMaybe 2 (_nfcDecimals config)
  dragRate
    | isIntegral selValue = fromMaybe 1 (_nfcDragRate config)
    | otherwise = fromMaybe 0.1 (_nfcDragRate config)
  minVal = _nfcMinValue config
  maxVal = _nfcMaxValue config
  fromText = numberFromText minVal maxVal
  toText = numberToText
  tmpValue = selValue + fromFractional (dy * dragRate)
  mParsedVal = fromText (toText decimals tmpValue)
  parsedVal = fromJust mParsedVal
  newVal
    | isJust mParsedVal = parsedVal
    | dy > 0 && isJust maxVal = fromJust maxVal
    | dy < 0 && isJust minVal = fromJust minVal
    | otherwise = _ifsCurrValue state
  newText = toText decimals newVal
  newPos = _ifsCursorPos state
  newSel = _ifsSelStart state
  result = (newText, newPos, newSel)

numberFromText :: FormattableNumber a => Maybe a -> Maybe a -> Text -> Maybe a
numberFromText minVal maxVal t = case signed rational t of
  Right (frac, _)
    | numberInBounds minVal maxVal val -> Just val
    where
      val = fromFractional frac
  _ -> Nothing

numberToText :: FormattableNumber a => Int -> a -> Text
numberToText decimals val = F.sformat (F.fixed decimals) val

acceptNumberInput :: Int -> Text -> Bool
acceptNumberInput decimals text = isRight (A.parseOnly parser text) where
  sign = A.option "" (single '-')
  number = A.takeWhile isDigit
  digit = T.singleton <$> A.digit
  dot = single '.'
  dots = if decimals > 0 then 1 else 0
  rest = join [upto dots dot, upto decimals digit]
  parser = join [sign, number, A.option "" rest] <* A.endOfInput

-- Parsing helpers
join :: [A.Parser Text] -> A.Parser Text
join [] = return T.empty
join (x:xs) = (<>) <$> x <*> join xs

upto :: Int -> A.Parser Text -> A.Parser Text
upto n p
  | n > 0 = (<>) <$> A.try p <*> upto (n-1) p <|> return T.empty
  | otherwise = return T.empty

single :: Char -> A.Parser Text
single c = T.singleton <$> A.char c

numberInBounds :: (Ord a, Num a) => Maybe a -> Maybe a -> a -> Bool
numberInBounds Nothing Nothing _ = True
numberInBounds (Just minVal) Nothing val = val >= minVal
numberInBounds Nothing (Just maxVal) val = val <= maxVal
numberInBounds (Just minVal) (Just maxVal) val = val >= minVal && val <= maxVal

isIntegral :: Typeable a => a -> Bool
isIntegral val
  | "Int" `isPrefixOf` name = True
  | "Word" `isPrefixOf` name = True
  | otherwise = False
  where
    name = show (typeOf val)
