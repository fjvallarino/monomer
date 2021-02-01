{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Keystroke (
  keystroke,
  keystroke_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~), (%~), at)
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Bifunctor (first)
import Data.Char (chr, isAscii, isPrint, ord)
import Data.Default
import Data.List (foldl')
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T

import Monomer.Widgets.Container

import qualified Monomer.Lens as L

newtype KeystrokeCfg = KeystrokeCfg {
  _kscIgnoreChildren :: Maybe Bool
}

instance Default KeystrokeCfg where
  def = KeystrokeCfg {
    _kscIgnoreChildren = Nothing
  }

instance Semigroup KeystrokeCfg where
  (<>) t1 t2 = KeystrokeCfg {
    _kscIgnoreChildren = _kscIgnoreChildren t2 <|> _kscIgnoreChildren t1
  }

instance Monoid KeystrokeCfg where
  mempty = def

instance CmbIgnoreChildrenEvts KeystrokeCfg where
  ignoreChildrenEvts = def {
    _kscIgnoreChildren = Just True
  }

data KeyStroke = KeyStroke {
  _kstKsC :: Bool,
  _kstKsCtrl :: Bool,
  _kstKsCmd :: Bool,
  _kstKsAlt :: Bool,
  _kstKsShift :: Bool,
  _kstKsKeys :: Set KeyCode
} deriving (Eq, Show)

instance Default KeyStroke where
  def = KeyStroke {
    _kstKsC = False,
    _kstKsCtrl = False,
    _kstKsCmd = False,
    _kstKsAlt = False,
    _kstKsShift = False,
    _kstKsKeys = Set.empty
  }

makeLensesWith abbreviatedFields ''KeyStroke

keystroke :: [(Text, e)] -> WidgetNode s e -> WidgetNode s e
keystroke bindings managed = keystroke_ bindings def managed

keystroke_
  :: [(Text, e)] -> [KeystrokeCfg] -> WidgetNode s e -> WidgetNode s e
keystroke_ bindings configs managed = makeNode widget managed where
  config = mconcat configs
  newBindings = fmap (first textToStroke) bindings
  widget = makeKeystroke newBindings config

makeNode :: Widget s e -> WidgetNode s e -> WidgetNode s e
makeNode widget managedWidget = defaultWidgetNode "keystroke" widget
  & L.info . L.focusable .~ False
  & L.children .~ Seq.singleton managedWidget

makeKeystroke :: [(KeyStroke, e)] -> KeystrokeCfg -> Widget s e
makeKeystroke bindings config = widget where
  widget = createContainer () def {
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  handleEvent wenv target evt node = case evt of
    KeyAction mod code KeyPressed -> Just result where
      ignoreChildren = Just True == _kscIgnoreChildren config
      newWenv = wenv & L.inputStatus %~ removeMods
      evts = snd <$> filter (keyStrokeActive newWenv code . fst) bindings
      reqs
        | ignoreChildren && not (null evts) = [IgnoreChildrenEvents]
        | otherwise = []
      result = resultReqsEvts node reqs evts
    _ -> Nothing

  getSizeReq :: ContainerGetSizeReqHandler s e a
  getSizeReq wenv currState node children = (newReqW, newReqH) where
    child = Seq.index children 0
    newReqW = child ^. L.info . L.sizeReqW
    newReqH = child ^. L.info . L.sizeReqH

  resize :: ContainerResizeHandler s e
  resize wenv renderArea children node = resized where
    style = activeStyle wenv node
    contentArea = fromMaybe def (removeOuterBounds style renderArea)
    resized = (resultWidget node, Seq.singleton contentArea)

keyStrokeActive :: WidgetEnv s e -> KeyCode -> KeyStroke -> Bool
keyStrokeActive wenv code ks = currValid && allPressed && validMods where
  status = wenv ^. L.inputStatus
  keyMod = status ^. L.keyMod
  isPressed code = Just KeyPressed == M.lookup code (status ^. L.keys)
  currValid = code `elem` (ks ^. ksKeys) || code `elem` modKeys
  pressedKeys = M.filter (== KeyPressed) (status ^. L.keys)
  allPressed = M.keysSet pressedKeys == ks ^. ksKeys
  ctrlPressed = isCtrlPressed keyMod
  cmdPressed = isMacOS wenv && isGUIPressed keyMod
  validC = not (ks ^. ksC) || ks ^. ksC == (ctrlPressed || cmdPressed)
  validCtrl = ks ^. ksCtrl == ctrlPressed || ctrlPressed && validC
  validCmd = ks ^. ksCmd == cmdPressed || cmdPressed && validC
  validShift = ks ^. ksShift == isShiftPressed keyMod
  validAlt = ks ^. ksAlt == isAltPressed keyMod
  validMods = (validC && validCtrl && validCmd) && validShift && validAlt

textToStroke :: Text -> KeyStroke
textToStroke text = ks where
  parts = T.split (=='-') text
  ks = foldl' partToStroke def parts

partToStroke :: KeyStroke -> Text -> KeyStroke
partToStroke ks "A" = ks & ksAlt .~ True
partToStroke ks "Alt" = ks & ksAlt .~ True
partToStroke ks "C" = ks & ksC .~ True
partToStroke ks "Ctrl" = ks & ksCtrl .~ True
partToStroke ks "Cmd" = ks & ksCmd .~ True
partToStroke ks "O" = ks & ksAlt .~ True
partToStroke ks "Option" = ks & ksAlt .~ True
partToStroke ks "S" = ks & ksShift .~ True
partToStroke ks "Shift" = ks & ksShift .~ True
-- Main keys
partToStroke ks "Caps" = ks & ksKeys %~ Set.insert keyCapsLock
partToStroke ks "Delete" = ks & ksKeys %~ Set.insert keyDelete
partToStroke ks "Enter" = ks & ksKeys %~ Set.insert keyReturn
partToStroke ks "Esc" = ks & ksKeys %~ Set.insert keyEscape
partToStroke ks "Return" = ks & ksKeys %~ Set.insert keyReturn
partToStroke ks "Space" = ks & ksKeys %~ Set.insert keySpace
partToStroke ks "Tab" = ks & ksKeys %~ Set.insert keyTab
-- Arrows
partToStroke ks "Up" = ks & ksKeys %~ Set.insert keyUp
partToStroke ks "Down" = ks & ksKeys %~ Set.insert keyDown
partToStroke ks "Left" = ks & ksKeys %~ Set.insert keyLeft
partToStroke ks "Right" = ks & ksKeys %~ Set.insert keyRight
-- Function keys
partToStroke ks "F1" = ks & ksKeys %~ Set.insert keyF1
partToStroke ks "F2" = ks & ksKeys %~ Set.insert keyF2
partToStroke ks "F3" = ks & ksKeys %~ Set.insert keyF3
partToStroke ks "F4" = ks & ksKeys %~ Set.insert keyF4
partToStroke ks "F5" = ks & ksKeys %~ Set.insert keyF5
partToStroke ks "F6" = ks & ksKeys %~ Set.insert keyF6
partToStroke ks "F7" = ks & ksKeys %~ Set.insert keyF7
partToStroke ks "F8" = ks & ksKeys %~ Set.insert keyF8
partToStroke ks "F9" = ks & ksKeys %~ Set.insert keyF9
partToStroke ks "F10" = ks & ksKeys %~ Set.insert keyF10
partToStroke ks "F11" = ks & ksKeys %~ Set.insert keyF11
partToStroke ks "F12" = ks & ksKeys %~ Set.insert keyF12
-- Other keys (numbers, letters, points, etc)
partToStroke ks txt
  | isValid = ks & ksKeys %~ Set.insert (KeyCode (ord txtHead))
  | otherwise = ks
  where
    isValid = T.length txt == 1 && isAscii txtHead && isPrint txtHead
    txtHead = T.index txt 0

removeMods :: InputStatus -> InputStatus
removeMods status = status
  & L.keys %~ M.filterWithKey (\k v -> k `notElem` modKeys)

modKeys :: [KeyCode]
modKeys = [
    keyLAlt, keyRAlt, keyLCtrl, keyRCtrl, keyLGUI, keyRGUI, keyLShift, keyRShift
  ]
