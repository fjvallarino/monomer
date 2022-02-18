{-|
Module      : Monomer.Widgets.Singles.ExternalLink
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Provides a clickable link that opens in the system's browser. It uses OS
services to open the URI, which means not only URLs can be opened.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Monomer.Widgets.Singles.ExternalLink (
  -- * Configuration
  ExternalLinkCfg,
  -- * Constructors
  externalLink,
  externalLink_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Maybe
import Data.Text (Text)
import System.Process (callCommand)

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Helper (catchAny)
import Monomer.Widgets.Container
import Monomer.Widgets.Singles.Label

import qualified Monomer.Lens as L

{-|
Configuration options for externalLink:

- 'trimSpaces': whether to remove leading/trailing spaces in the caption.
- 'ellipsis': if ellipsis should be used for overflown text.
- 'multiline': if text may be split in multiple lines.
- 'lineBreak': how to break texts into lines.
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
data ExternalLinkCfg s e = ExternalLinkCfg {
  _elcLabelCfg :: LabelCfg s e,
  _elcOnFocusReq :: [Path -> WidgetRequest s e],
  _elcOnBlurReq :: [Path -> WidgetRequest s e]
}

instance Default (ExternalLinkCfg s e) where
  def = ExternalLinkCfg {
    _elcLabelCfg = def,
    _elcOnFocusReq = [],
    _elcOnBlurReq = []
  }

instance Semigroup (ExternalLinkCfg s e) where
  (<>) t1 t2 = ExternalLinkCfg {
    _elcLabelCfg = _elcLabelCfg t1 <> _elcLabelCfg t2,
    _elcOnFocusReq = _elcOnFocusReq t1 <> _elcOnFocusReq t2,
    _elcOnBlurReq = _elcOnBlurReq t1 <> _elcOnBlurReq t2
  }

instance Monoid (ExternalLinkCfg s e) where
  mempty = def

instance CmbTrimSpaces (ExternalLinkCfg s e) where
  trimSpaces_ trim = def {
    _elcLabelCfg = trimSpaces_ trim
  }

instance CmbEllipsis (ExternalLinkCfg s e) where
  ellipsis_ ellipsis = def {
    _elcLabelCfg = ellipsis_ ellipsis
  }

instance CmbMultiline (ExternalLinkCfg s e) where
  multiline_ multi = def {
    _elcLabelCfg = multiline_ multi
  }

instance CmbLineBreak (ExternalLinkCfg s e) where
  lineBreak l = def {
    _elcLabelCfg = lineBreak l
  }

instance CmbMaxLines (ExternalLinkCfg s e) where
  maxLines count = def {
    _elcLabelCfg = maxLines count
  }

instance CmbResizeFactor (ExternalLinkCfg s e) where
  resizeFactor s = def {
    _elcLabelCfg = resizeFactor s
  }

instance CmbResizeFactorDim (ExternalLinkCfg s e) where
  resizeFactorW w = def {
    _elcLabelCfg = resizeFactorW w
  }
  resizeFactorH h = def {
    _elcLabelCfg = resizeFactorH h
  }

instance WidgetEvent e => CmbOnFocus (ExternalLinkCfg s e) e Path where
  onFocus fn = def {
    _elcOnFocusReq = [RaiseEvent . fn]
  }

instance CmbOnFocusReq (ExternalLinkCfg s e) s e Path where
  onFocusReq req = def {
    _elcOnFocusReq = [req]
  }

instance WidgetEvent e => CmbOnBlur (ExternalLinkCfg s e) e Path where
  onBlur fn = def {
    _elcOnBlurReq = [RaiseEvent . fn]
  }

instance CmbOnBlurReq (ExternalLinkCfg s e) s e Path where
  onBlurReq req = def {
    _elcOnBlurReq = [req]
  }

-- | Creates an external link with the given caption and url.
externalLink :: WidgetEvent e => Text -> Text -> WidgetNode s e
externalLink caption url = externalLink_ caption url def

-- | Creates an external link with the given caption and url. Accepts config.
externalLink_
  :: WidgetEvent e => Text -> Text -> [ExternalLinkCfg s e] -> WidgetNode s e
externalLink_ caption url configs = externalLinkNode where
  config = mconcat configs
  widget = makeExternalLink caption url config
  externalLinkNode = defaultWidgetNode "externalLink" widget
    & L.info . L.focusable .~ True

makeExternalLink
  :: WidgetEvent e => Text -> Text -> ExternalLinkCfg s e -> Widget s e
makeExternalLink !caption !url !config = widget where
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

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.externalLinkStyle

  createChildNode wenv node = newNode where
    nodeStyle = node ^. L.info . L.style
    labelCfg = _elcLabelCfg config
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
    Focus prev -> handleFocusChange node prev (_elcOnFocusReq config)

    Blur next -> handleFocusChange node next (_elcOnBlurReq config)

    KeyAction mode code status
      | isSelectKey code && status == KeyPressed -> Just result
      where
        isSelectKey code = isKeyReturn code || isKeySpace code

    Click p _ _
      | isPointInNodeVp node p -> Just result

    ButtonAction p btn BtnPressed 1 -- Set focus on click
      | mainBtn btn && pointInVp p && not focused -> Just resultFocus

    ButtonAction p btn BtnReleased clicks
      | mainBtn btn && focused && pointInVp p && clicks > 1 -> Just result
    _ -> Nothing
    where
      widgetId = node ^. L.info . L.widgetId
      path = node ^. L.info . L.path
      mainBtn btn = btn == wenv ^. L.mainButton

      focused = isNodeFocused wenv node
      pointInVp p = isPointInNodeVp node p
      openLinkTask = openLink wenv (T.unpack url)

      requests = [RunTask widgetId path openLinkTask]
      result = resultReqs node requests
      resultFocus = resultReqs node [SetFocus (node ^. L.info . L.widgetId)]

  resize wenv node viewport children = resized where
    assignedAreas = Seq.fromList [viewport]
    resized = (resultNode node, assignedAreas)

openLink :: WidgetEnv s e -> String -> IO ()
openLink wenv url = catchIgnore (callCommand openCommand) where
  os = wenv ^. L.os
  command
    | os == "Windows" = "start"
    | os == "Mac OS X" = "open"
    | os == "Linux" = "xdg-open"
    | otherwise = "ls"
  openCommand = command ++ " \"" ++ url ++ "\""

catchIgnore :: IO () -> IO ()
catchIgnore task = catchAny task (const $ return ())
