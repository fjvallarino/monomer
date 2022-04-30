{-|
Module      : Monomer.Widgets.Singles.Button
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Button widget, with support for multiline text. At the most basic level, a
button consists of a caption and an event to raise when clicked.

@
button "Increase count" AppIncrease
@
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Monomer.Widgets.Singles.Button (
  -- * Configuration
  ButtonCfg,
  -- * Constructors
  mainButton,
  mainButton_,
  mainButtonD_,
  button,
  button_,
  buttonD_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Maybe
import Data.Text (Text)

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container
import Monomer.Widgets.Singles.Label

import qualified Monomer.Lens as L

data ButtonType
  = ButtonNormal
  | ButtonMain
  deriving (Eq, Show)

{-|
Configuration options for button:

- 'ignoreParentEvts': whether to ignore all other responses to the click or
  keypress that triggered the button, and only keep this button's response.
  Useful when the button is child of a _keystroke_ widget.
- 'trimSpaces': whether to remove leading/trailing spaces in the caption.
- 'ellipsis': if ellipsis should be used for overflown text.
- 'multiline': if text may be split in multiple lines.
- 'maxLines': maximum number of text lines to show.
- 'resizeFactor': flexibility to have more or less spaced assigned.
- 'resizeFactorW': flexibility to have more or less horizontal spaced assigned.
- 'resizeFactorH': flexibility to have more or less vertical spaced assigned.
- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onClick': event to raise when button is clicked.
- 'onClickReq': 'WidgetRequest' to generate when button is clicked.
-}
data ButtonCfg s e = ButtonCfg {
  _btnButtonType :: Maybe ButtonType,
  _btnIgnoreParent :: Maybe Bool,
  _btnIgnoreTheme :: Maybe Bool,
  _btnLabelCfg :: LabelCfg s e,
  _btnOnFocusReq :: [Path -> WidgetRequest s e],
  _btnOnBlurReq :: [Path -> WidgetRequest s e],
  _btnOnClickReq :: [WidgetRequest s e]
}

instance Default (ButtonCfg s e) where
  def = ButtonCfg {
    _btnButtonType = Nothing,
    _btnIgnoreParent = Nothing,
    _btnIgnoreTheme = Nothing,
    _btnLabelCfg = def,
    _btnOnFocusReq = [],
    _btnOnBlurReq = [],
    _btnOnClickReq = []
  }

instance Semigroup (ButtonCfg s e) where
  (<>) t1 t2 = ButtonCfg {
    _btnButtonType = _btnButtonType t2 <|> _btnButtonType t1,
    _btnIgnoreParent = _btnIgnoreParent t2 <|> _btnIgnoreParent t1,
    _btnIgnoreTheme = _btnIgnoreTheme t2 <|> _btnIgnoreTheme t1,
    _btnLabelCfg = _btnLabelCfg t1 <> _btnLabelCfg t2,
    _btnOnFocusReq = _btnOnFocusReq t1 <> _btnOnFocusReq t2,
    _btnOnBlurReq = _btnOnBlurReq t1 <> _btnOnBlurReq t2,
    _btnOnClickReq = _btnOnClickReq t1 <> _btnOnClickReq t2
  }

instance Monoid (ButtonCfg s e) where
  mempty = def

instance CmbIgnoreParentEvts (ButtonCfg s e) where
  ignoreParentEvts_ ignore = def {
    _btnIgnoreParent = Just ignore
  }

instance CmbIgnoreTheme (ButtonCfg s e) where
  ignoreTheme_ ignore = def {
    _btnIgnoreTheme = Just ignore
  }

instance CmbTrimSpaces (ButtonCfg s e) where
  trimSpaces_ trim = def {
    _btnLabelCfg = trimSpaces_ trim
  }

instance CmbEllipsis (ButtonCfg s e) where
  ellipsis_ ellipsis = def {
    _btnLabelCfg = ellipsis_ ellipsis
  }

instance CmbMultiline (ButtonCfg s e) where
  multiline_ multi = def {
    _btnLabelCfg = multiline_ multi
  }

instance CmbMaxLines (ButtonCfg s e) where
  maxLines count = def {
    _btnLabelCfg = maxLines count
  }

instance CmbResizeFactor (ButtonCfg s e) where
  resizeFactor s = def {
    _btnLabelCfg = resizeFactor s
  }

instance CmbResizeFactorDim (ButtonCfg s e) where
  resizeFactorW w = def {
    _btnLabelCfg = resizeFactorW w
  }
  resizeFactorH h = def {
    _btnLabelCfg = resizeFactorH h
  }

instance WidgetEvent e => CmbOnFocus (ButtonCfg s e) e Path where
  onFocus fn = def {
    _btnOnFocusReq = [RaiseEvent . fn]
  }

instance CmbOnFocusReq (ButtonCfg s e) s e Path where
  onFocusReq req = def {
    _btnOnFocusReq = [req]
  }

instance WidgetEvent e => CmbOnBlur (ButtonCfg s e) e Path where
  onBlur fn = def {
    _btnOnBlurReq = [RaiseEvent . fn]
  }

instance CmbOnBlurReq (ButtonCfg s e) s e Path where
  onBlurReq req = def {
    _btnOnBlurReq = [req]
  }

instance WidgetEvent e => CmbOnClick (ButtonCfg s e) e where
  onClick handler = def {
    _btnOnClickReq = [RaiseEvent handler]
  }

instance CmbOnClickReq (ButtonCfg s e) s e where
  onClickReq req = def {
    _btnOnClickReq = [req]
  }

mainConfig :: ButtonCfg s e
mainConfig = def {
  _btnButtonType = Just ButtonMain
}

{-|
Creates a button with main styling. Useful to highlight an option, such as
"Accept", when multiple buttons are available.
-}
mainButton :: WidgetEvent e => Text -> e -> WidgetNode s e
mainButton caption handler = button_ caption handler [mainConfig]

{-|
Creates a button with main styling. Useful to highlight an option, such as
"Accept", when multiple buttons are available. Accepts config.
-}
mainButton_ :: WidgetEvent e => Text -> e -> [ButtonCfg s e] -> WidgetNode s e
mainButton_ caption handler configs = button_ caption handler newConfigs where
  newConfigs = mainConfig : configs

{-|
Creates a button with main styling. Useful to highlight an option, such as
"Accept", when multiple buttons are available. Accepts config but does not
require an event. See 'buttonD_'.
-}
mainButtonD_ :: WidgetEvent e => Text -> [ButtonCfg s e] -> WidgetNode s e
mainButtonD_ caption configs = buttonD_ caption newConfigs where
  newConfigs = mainConfig : configs

-- | Creates a button with normal styling.
button :: WidgetEvent e => Text -> e -> WidgetNode s e
button caption handler = button_ caption handler def

-- | Creates a button with normal styling. Accepts config.
button_ :: WidgetEvent e => Text -> e -> [ButtonCfg s e] -> WidgetNode s e
button_ caption handler configs = buttonNode where
  buttonNode = buttonD_ caption (onClick handler : configs)

{-|
Creates a button without forcing an event to be provided. The other constructors
use this version, adding an 'onClick' handler in configs.

Using this constructor directly can be helpful in cases where the event to be
raised belongs in a _Composite_ above in the widget tree, outside the scope of
the Composite that contains the button. This parent Composite can be reached by
sending a message ('SendMessage') to its 'WidgetId' using 'onClickReq'.
-}
buttonD_ :: WidgetEvent e => Text -> [ButtonCfg s e] -> WidgetNode s e
buttonD_ caption configs = buttonNode where
  config = mconcat configs
  widget = makeButton caption config
  !buttonNode = defaultWidgetNode "button" widget
    & L.info . L.focusable .~ True

makeButton :: WidgetEvent e => Text -> ButtonCfg s e -> Widget s e
makeButton !caption !config = widget where
  widget = createContainer () def {
    containerAddStyleReq = False,
    containerDrawDecorations = False,
    containerUseScissor = True,
    containerGetBaseStyle = getBaseStyle,
    containerInit = init,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerResize = resize
  }

  !buttonType = fromMaybe ButtonNormal (_btnButtonType config)

  getBaseStyle wenv node
    | ignoreTheme = Nothing
    | otherwise = case buttonType of
        ButtonNormal -> Just (collectTheme wenv L.btnStyle)
        ButtonMain -> Just (collectTheme wenv L.btnMainStyle)
    where
      ignoreTheme = _btnIgnoreTheme config == Just True

  createChildNode wenv node = newNode where
    nodeStyle = node ^. L.info . L.style
    labelCfg = _btnLabelCfg config
    labelCurrStyle = labelCurrentStyle childOfFocusedStyle
    !labelNode = label_ caption [ignoreTheme, labelCfg, labelCurrStyle]
      & L.info . L.style .~ nodeStyle
    !newNode = node
      & L.children .~ Seq.singleton labelNode

  init wenv node = result where
    result = resultNode (createChildNode wenv node)

  merge wenv node oldNode oldState = result where
    result = resultNode (createChildNode wenv node)

  handleEvent wenv node target evt = case evt of
    Focus prev -> handleFocusChange node prev (_btnOnFocusReq config)
    Blur next -> handleFocusChange node next (_btnOnBlurReq config)

    KeyAction mode code status
      | isSelectKey code && status == KeyPressed -> Just result
      where
        isSelectKey code = isKeyReturn code || isKeySpace code

    Click p _ _
      | isPointInNodeVp node p -> Just result

    ButtonAction p btn BtnPressed 1 -- Set focus on click
      | mainBtn btn && pointInVp p && not focused -> Just resultFocus

    _ -> Nothing
    where
      mainBtn btn = btn == wenv ^. L.mainButton

      focused = isNodeFocused wenv node
      pointInVp p = isPointInNodeVp node p
      ignoreParent = _btnIgnoreParent config == Just True

      reqs = _btnOnClickReq config ++ [IgnoreParentEvents | ignoreParent]
      result = resultReqs node reqs
      resultFocus = resultReqs node [SetFocus (node ^. L.info . L.widgetId)]

  resize wenv node viewport children = resized where
    assignedAreas = Seq.fromList [viewport]
    resized = (resultNode node, assignedAreas)
