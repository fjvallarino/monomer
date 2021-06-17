{-|
Module      : Monomer.Widgets.Singles.ExternalLink
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Provides a clickable link that opens in the system's browser. It uses OS
services to open the URI, which means not only URLs can be opened.

Configs:

- trimSpaces: whether to remove leading/trailing spaces in the caption.
- ellipsis: if ellipsis should be used for overflown text.
- multiLine: if text may be split in multiple lines.
- maxLines: maximum number of text lines to show.
- onFocus: event to raise when focus is received.
- onFocusReq: WidgetRequest to generate when focus is received.
- onBlur: event to raise when focus is lost.
- onBlurReq: WidgetRequest to generate when focus is lost.
- onClick: event to raise when button is clicked.
- onClickReq: WidgetRequest to generate when button is clicked.
- resizeFactor: flexibility to have more or less spaced assigned.
- resizeFactorW: flexibility to have more or less horizontal spaced assigned.
- resizeFactorH: flexibility to have more or less vertical spaced assigned.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Singles.ExternalLink (
  externalLink,
  externalLink_
) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, catch)
import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Maybe
import Data.Text (Text)
import System.Process (callCommand)

import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Widgets.Container
import Monomer.Widgets.Singles.Label

import qualified Monomer.Lens as L

data ExternalLinkCfg s e = ExternalLinkCfg {
  _elcTextTrim :: Maybe Bool,
  _elcTextEllipsis :: Maybe Bool,
  _elcTextMultiLine :: Maybe Bool,
  _elcTextMaxLines :: Maybe Int,
  _elcFactorW :: Maybe Double,
  _elcFactorH :: Maybe Double,
  _elcOnFocusReq :: [Path -> WidgetRequest s e],
  _elcOnBlurReq :: [Path -> WidgetRequest s e]
}

instance Default (ExternalLinkCfg s e) where
  def = ExternalLinkCfg {
    _elcTextTrim = Nothing,
    _elcTextEllipsis = Nothing,
    _elcTextMultiLine = Nothing,
    _elcTextMaxLines = Nothing,
    _elcFactorW = Nothing,
    _elcFactorH = Nothing,
    _elcOnFocusReq = [],
    _elcOnBlurReq = []
  }

instance Semigroup (ExternalLinkCfg s e) where
  (<>) t1 t2 = ExternalLinkCfg {
    _elcTextTrim = _elcTextTrim t2 <|> _elcTextTrim t1,
    _elcTextEllipsis = _elcTextEllipsis t2 <|> _elcTextEllipsis t1,
    _elcTextMultiLine = _elcTextMultiLine t2 <|> _elcTextMultiLine t1,
    _elcTextMaxLines = _elcTextMaxLines t2 <|> _elcTextMaxLines t1,
    _elcFactorW = _elcFactorW t2 <|> _elcFactorW t1,
    _elcFactorH = _elcFactorH t2 <|> _elcFactorH t1,
    _elcOnFocusReq = _elcOnFocusReq t1 <> _elcOnFocusReq t2,
    _elcOnBlurReq = _elcOnBlurReq t1 <> _elcOnBlurReq t2
  }

instance Monoid (ExternalLinkCfg s e) where
  mempty = def

instance CmbTrimSpaces (ExternalLinkCfg s e) where
  trimSpaces_ trim = def {
    _elcTextTrim = Just trim
  }

instance CmbEllipsis (ExternalLinkCfg s e) where
  ellipsis_ ellipsis = def {
    _elcTextEllipsis = Just ellipsis
  }

instance CmbMultiLine (ExternalLinkCfg s e) where
  multiLine_ multi = def {
    _elcTextMultiLine = Just multi
  }

instance CmbMaxLines (ExternalLinkCfg s e) where
  maxLines count = def {
    _elcTextMaxLines = Just count
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

instance CmbResizeFactor (ExternalLinkCfg s e) where
  resizeFactor s = def {
    _elcFactorW = Just s,
    _elcFactorH = Just s
  }

instance CmbResizeFactorDim (ExternalLinkCfg s e) where
  resizeFactorW w = def {
    _elcFactorW = Just w
  }
  resizeFactorH h = def {
    _elcFactorH = Just h
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
makeExternalLink caption url config = widget where
  widget = createContainer () def {
    containerUseScissor = True,
    containerGetBaseStyle = getBaseStyle,
    containerInit = init,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }

  trim = _elcTextTrim config == Just True
  ellipsis = _elcTextEllipsis config == Just True
  multiLine = _elcTextMultiLine config == Just True
  maxLinesV = _elcTextMaxLines config
  factorW = _elcFactorW config
  factorH = _elcFactorH config

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.externalLinkStyle

  createChildNode wenv node = newNode where
    nodeStyle = node ^. L.info . L.style
    labelStyle = collectStyleField_ L.text nodeStyle def
      & collectStyleField_ L.sizeReqW nodeStyle
      & collectStyleField_ L.sizeReqH nodeStyle
    cfgs = [
      ignoreTheme,
      trimSpaces_ trim,
      ellipsis_ ellipsis,
      multiLine_ multiLine]
      ++ [maxLines (fromJust maxLinesV) | isJust maxLinesV]
      ++ [resizeFactorW (fromJust factorW) | isJust factorW]
      ++ [resizeFactorH (fromJust factorH) | isJust factorH]
    labelNode = label_ caption cfgs
      & L.info . L.style .~ labelStyle
    childNode = labelNode
    newNode = node
      & L.children .~ Seq.singleton childNode

  init wenv node = result where
    result = resultNode (createChildNode wenv node)

  merge wenv node oldNode oldState = result where
    result = resultNode (createChildNode wenv node)

  handleEvent wenv node target evt = case evt of
    Focus prev -> handleFocusChange (_elcOnFocusReq config) prev node
    Blur next -> handleFocusChange (_elcOnBlurReq config) next node
    KeyAction mode code status
      | isSelectKey code && status == KeyPressed -> Just result
      where
        isSelectKey code = isKeyReturn code || isKeySpace code
    Click p _
      | isPointInNodeVp p node -> Just result
    -- Set focus on click
    ButtonAction p btn BtnPressed 1
      | mainBtn btn && pointInVp p && not focused -> Just resultFocus
    ButtonAction p btn BtnReleased clicks
      | mainBtn btn && focused && pointInVp p && clicks > 1 -> Just result
    _ -> Nothing
    where
      widgetId = node ^. L.info . L.widgetId
      path = node ^. L.info . L.path
      mainBtn btn = btn == wenv ^. L.mainButton
      focused = isNodeFocused wenv node
      pointInVp p = isPointInNodeVp p node
      openLinkTask = openLink wenv (T.unpack url)
      requests = [RunTask widgetId path openLinkTask]
      result = resultReqs node requests
      resultFocus = resultReqs node [SetFocus (node ^. L.info . L.widgetId)]

  getSizeReq :: ContainerGetSizeReqHandler s e
  getSizeReq wenv node children = (newReqW, newReqH) where
    -- Main section reqs
    child = Seq.index children 0
    newReqW = child ^. L.info . L.sizeReqW
    newReqH = child ^. L.info . L.sizeReqH

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

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch
