{-# LANGUAGE RankNTypes #-}
module Monomer.Widgets.Dialog where

import Debug.Trace

import Control.Lens (Lens', (&), (^.), (^?), (.~), (?~), (<>~), non)
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Typeable (Typeable)

import Monomer.Core
import Monomer.Event
import Monomer.Graphics

import Monomer.Widgets.Box
import Monomer.Widgets.Button
import Monomer.Widgets.Container
import Monomer.Widgets.Label
import Monomer.Widgets.Spacer
import Monomer.Widgets.Stack

import qualified Monomer.Lens as L

--newtype DialogCfg e = DialogCfg {
--  _dgcButtons :: [(Text, e)]
--}
createThemed
  :: WidgetType
  -> (WidgetEnv s e -> WidgetInstance s e)
  -> WidgetInstance s e
createThemed widgetType factory = newInst where
  --themedInit wenv inst = resultWidget $ factory wenv
  createInst wenv inst = resultWidget themedInst where
    tempInst = factory wenv
    themedInst = inst {
      _wiWidget = _wiWidget tempInst,
      _wiChildren = _wiChildren tempInst,
      _wiFocusable = _wiFocusable tempInst,
      _wiStyle = _wiStyle tempInst <> _wiStyle inst
    }
  init = createInst
  merge wenv oldState inst = createInst wenv inst
  newWidget = createContainer def {
    containerInit = createInst,
    containerMerge = merge
  }
  newInst = defaultWidgetInstance widgetType newWidget

alert :: Text -> Text -> Text -> e -> WidgetInstance s e
alert title message caption evt = createThemed "alert" factory where
  factory wenv = makeAlert wenv title message caption evt

-- Maybe add styles for dialog and color for inactive/empty background
makeAlert
  :: WidgetEnv s e -> Text -> Text -> Text -> e -> WidgetInstance s e
makeAlert wenv title message caption evt = alertBox where
  emptyOverlayColor = themeEmptyOverlayColor wenv
  dismissButton = button caption evt & L.style .~ themeBtnMain wenv
  alertTree = vstack [
      label title & L.style .~ themeDialogTitle wenv,
      label message & L.style .~ themeDialogBody wenv,
      box_ dismissButton [alignLeft] & L.style .~ themeDialogButtons wenv
    ] & L.style .~ themeDialogFrame wenv
  alertBox = box_ alertTree [onClickEmpty evt] & L.style .~ emptyOverlayColor

themeEmptyOverlayColor :: WidgetEnv s e -> Style
themeEmptyOverlayColor wenv = copyThemeField wenv def L.bgColor L.emptyOverlayColor

themeText :: WidgetEnv s e -> Style
themeText wenv = copyThemeField wenv def L.text L.text

themeBtn :: WidgetEnv s e -> Style
themeBtn wenv = collectTheme wenv L.btnStyle

themeBtnMain :: WidgetEnv s e -> Style
themeBtnMain wenv = collectTheme wenv L.btnMainStyle

themeDialogFrame :: WidgetEnv s e -> Style
themeDialogFrame wenv = collectTheme wenv L.dialogFrameStyle

themeDialogTitle :: WidgetEnv s e -> Style
themeDialogTitle wenv = collectTheme wenv L.dialogTitleStyle

themeDialogBody :: WidgetEnv s e -> Style
themeDialogBody wenv = collectTheme wenv L.dialogBodyStyle

themeDialogButtons :: WidgetEnv s e -> Style
themeDialogButtons wenv = collectTheme wenv L.dialogButtonsStyle

copyThemeField
  :: WidgetEnv s e
  -> Style
  -> Lens' StyleState (Maybe t)
  -> Lens' ThemeState t
  -> Style
copyThemeField wenv base fieldS fieldT = style where
  basic = Just $ base ^. L.basic . non def
    & fieldS ?~ wenv ^. L.theme . L.basic . fieldT
  hover = Just $ base ^. L.hover . non def
    & fieldS ?~ wenv ^. L.theme . L.hover . fieldT
  focus = Just $ base ^. L.focus . non def
    & fieldS ?~ wenv ^. L.theme . L.focus . fieldT
  disabled = Just $ base ^. L.disabled . non def
    & fieldS ?~ wenv ^. L.theme . L.disabled . fieldT
  style = Style basic hover focus disabled

collectTheme
  :: WidgetEnv s e  -> Lens' ThemeState StyleState
  -> Style
collectTheme wenv fieldT = style where
  basic = Just $ wenv ^. L.theme . L.basic . fieldT
  hover = Just $ wenv ^. L.theme . L.hover . fieldT
  focus = Just $ wenv ^. L.theme . L.focus . fieldT
  disabled = Just $ wenv ^. L.theme . L.disabled . fieldT
  style = Style basic hover focus disabled

makeConfirm :: Text -> Text -> WidgetInstance sd ed
makeConfirm title message = vstack [
    label title,
    label message
  ]
