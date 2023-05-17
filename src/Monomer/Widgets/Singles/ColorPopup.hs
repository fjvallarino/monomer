{-|
Module      : Monomer.Widgets.Singles.ColorPopup
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Color popup, displayed inside its parent container as a colored square. When
clicked, it opens a color picker overlay.

Shows sliders for the color components.

@
colorPopup colorLens
@

Optionally shows a slider for the alpha channel.

@
colorPopup_ colorLens [showAlpha]
@
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Singles.ColorPopup (
  -- * Constructors
  colorPopup,
  colorPopup_,
  colorPopupV,
  colorPopupV_
) where

import Control.Lens ((&), (^.), (.~), (?~), ALens', abbreviatedFields, makeLensesWith, non)
import Data.Default

import Monomer.Core.Combinators
import Monomer.Graphics.Types

import Monomer.Widgets.Composite
import Monomer.Widgets.Containers.BoxShadow
import Monomer.Widgets.Containers.Popup
import Monomer.Widgets.Singles.ColorPicker
import Monomer.Widgets.Singles.ToggleButton

import qualified Monomer.Lens as L

type ColorPopupEnv = WidgetEnv ColorPopupModel ColorPopupEvt
type ColorPopupNode = WidgetNode ColorPopupModel ColorPopupEvt

{-|
Configuration options for colorPopup:

- 'showAlpha': whether to allow modifying the alpha channel or not.
- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onChange': event to raise when any of the values changes.
- 'onChangeReq': 'WidgetRequest' to generate when any of the values changes.
-}
data ColorPopupCfg s e = ColorPopupCfg {
  _cpcColorPickerCfg :: ColorPickerCfg ColorPopupModel ColorPopupEvt,
  _cpcOnFocusReq :: [Path -> WidgetRequest s e],
  _cpcOnBlurReq :: [Path -> WidgetRequest s e],
  _cpcOnChangeReq :: [Color -> WidgetRequest s e]
}

instance Default (ColorPopupCfg s e) where
  def = ColorPopupCfg {
    _cpcColorPickerCfg = def,
    _cpcOnFocusReq = [],
    _cpcOnBlurReq = [],
    _cpcOnChangeReq = []
  }

instance Semigroup (ColorPopupCfg s e) where
  (<>) a1 a2 = def {
    _cpcColorPickerCfg = _cpcColorPickerCfg a1 <> _cpcColorPickerCfg a2,
    _cpcOnFocusReq = _cpcOnFocusReq a1 <> _cpcOnFocusReq a2,
    _cpcOnBlurReq = _cpcOnBlurReq a1 <> _cpcOnBlurReq a2,
    _cpcOnChangeReq = _cpcOnChangeReq a1 <> _cpcOnChangeReq a2
  }

instance Monoid (ColorPopupCfg s e) where
  mempty = def

instance CmbShowAlpha (ColorPopupCfg s e) where
  showAlpha_ show = def {
    _cpcColorPickerCfg = showAlpha_ show
  }

instance WidgetEvent e => CmbOnFocus (ColorPopupCfg s e) e Path where
  onFocus fn = def {
    _cpcOnFocusReq = [RaiseEvent . fn]
  }

instance CmbOnFocusReq (ColorPopupCfg s e) s e Path where
  onFocusReq req = def {
    _cpcOnFocusReq = [req]
  }

instance WidgetEvent e => CmbOnBlur (ColorPopupCfg s e) e Path where
  onBlur fn = def {
    _cpcOnBlurReq = [RaiseEvent . fn]
  }

instance CmbOnBlurReq (ColorPopupCfg s e) s e Path where
  onBlurReq req = def {
    _cpcOnBlurReq = [req]
  }

instance WidgetEvent e => CmbOnChange (ColorPopupCfg s e) Color e where
  onChange fn = def {
    _cpcOnChangeReq = [RaiseEvent . fn]
  }

instance CmbOnChangeReq (ColorPopupCfg s e) s e Color where
  onChangeReq req = def {
    _cpcOnChangeReq = [req]
  }

data ColorPopupModel = ColorPopupModel {
  _cpmPopupShowColor :: Bool,
  _cpmPopupColor :: Color
} deriving (Eq, Show)

data ColorPopupEvt
  = ColorChanged Color
  | PopupFocus Path
  | PopupBlur Path

instance Default ColorPopupModel where
  def = ColorPopupModel {
    _cpmPopupShowColor = False,
    _cpmPopupColor = def
  }

makeLensesWith abbreviatedFields 'ColorPopupModel

-- | Creates a colorPopup using the given lens.
colorPopup
  :: (WidgetModel s, WidgetEvent e)
  => ALens' s Color  -- ^ The lens into the model.
  -> WidgetNode s e  -- ^ The created color popup.
colorPopup field = colorPopup_ field def

-- | Creates a colorPopup using the given lens. Accepts config.
colorPopup_
  :: (WidgetModel s, WidgetEvent e)
  => ALens' s Color       -- ^ The lens into the model.
  -> [ColorPopupCfg s e]  -- ^ The config options.
  -> WidgetNode s e       -- ^ The created color popup.
colorPopup_ field configs = colorPopupD_ (WidgetLens field) configs

-- | Creates a colorPopup using the given value and 'onChange' event handler.
colorPopupV
  :: (WidgetModel s, WidgetEvent e)
  => Color           -- ^ The current value.
  -> (Color -> e)    -- ^ The event to raise on change.
  -> WidgetNode s e  -- ^ The created color popup.
colorPopupV value handler = colorPopupV_ value handler def

-- | Creates a colorPopup using the given value and 'onChange' event handler.
--   Accepts config.
colorPopupV_
  :: (WidgetModel s, WidgetEvent e)
  => Color                -- ^ The current value.
  -> (Color -> e)         -- ^ The event to raise on change.
  -> [ColorPopupCfg s e]  -- ^ The config options.
  -> WidgetNode s e       -- ^ The created color popup.
colorPopupV_ value handler configs = newNode where
  newConfigs = onChange handler : configs
  newNode = colorPopupD_ (WidgetValue value) newConfigs

-- | Creates a colorPopup providing a 'WidgetData' instance and config.
colorPopupD_
  :: (WidgetModel s, WidgetEvent e)
  => WidgetData s Color   -- ^ The 'WidgetData' to retrieve the value from.
  -> [ColorPopupCfg s e]  -- ^ The config options.
  -> WidgetNode s e       -- ^ The created color popup.
colorPopupD_ wdata configs = newNode where
  config = mconcat configs
  model = WidgetValue def
  uiBuilder = buildUI config
  eventHandler = handleEvent wdata config
  mergeModel wenv parentModel oldModel newModel = oldModel
    & popupColor .~ widgetDataGet parentModel wdata
  compCfg = [compositeMergeModel mergeModel]
  newNode = compositeD_ "colorPopup" model uiBuilder eventHandler compCfg

buildUI
  :: WidgetModel sp
  => ColorPopupCfg sp ep
  -> ColorPopupEnv
  -> ColorPopupModel
  -> ColorPopupNode
buildUI config wenv model = widgetTree where
  containerStyle = collectTheme wenv L.colorPopupStyle
  selColor = model ^. popupColor

  toggleStyle = mergeBasicStyle $ def
    & L.basic . non def . L.sizeReqW ?~ width 30
    & L.basic . non def . L.sizeReqH ?~ height 30
    & L.basic . non def . L.bgColor ?~ selColor
    & L.basic . non def . L.border ?~ border 1 selColor

  toggleCfg = [toggleButtonOffStyle toggleStyle]
  toggle = toggleButton_ "" popupShowColor toggleCfg
    & L.info . L.style .~ toggleStyle

  pickerCfg = _cpcColorPickerCfg config
  picker = colorPicker_ popupColor [pickerCfg, onChange ColorChanged]
    & L.info . L.style .~ containerStyle

  content = boxShadow picker
  popupCfg = [popupAlignToOuterV, popupOffset (Point 0 10), alignBottom, alignLeft]
  widgetTree = popup_ popupShowColor (popupAnchor toggle : popupCfg) content

handleEvent
  :: WidgetModel sp
  => WidgetData sp Color
  -> ColorPopupCfg sp ep
  -> ColorPopupEnv
  -> ColorPopupNode
  -> ColorPopupModel
  -> ColorPopupEvt
  -> [EventResponse ColorPopupModel ColorPopupEvt sp ep]
handleEvent wdata cfg wenv node model evt = case evt of
  PopupFocus prev
    | not (isNodeParentOfPath node prev) -> reportFocus prev
  PopupBlur next
    | not (isNodeParentOfPath node next) -> reportBlur next
  ColorChanged col -> reportChange col
  _ -> []
  where
    parentColor pm = widgetDataGet pm wdata
    parentChanged pm = parentColor pm /= model ^. popupColor

    report reqs = RequestParent <$> reqs
    reportFocus prev = report (($ prev) <$> _cpcOnFocusReq cfg)
    reportBlur next = report (($ next) <$> _cpcOnBlurReq cfg)
    reportChange col = report (wdataReqs ++ changeReqs) where
      wdataReqs = widgetDataSet wdata col
      changeReqs =  ($ col) <$> _cpcOnChangeReq cfg
