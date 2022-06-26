{-|
Module      : Monomer.Main.UserUtil
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Helper functions for Monomer users, to simplify common operations such as focus
change and clipboard requests.
-}
{-# LANGUAGE Strict #-}

module Monomer.Main.UserUtil where

import Control.Applicative ((<|>))
import Control.Lens
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer.Widgets.Composite
import Monomer.Widgets.Singles.Spacer

import qualified Monomer.Core.Lens as L
import qualified Monomer.Main.Lens as L

{-# DEPRECATED setFocusOnKey "Use SetFocusOnKey instead (wenv argument should be removed)." #-}
{-|
Generates a response to set focus on the given key, provided as WidgetKey. If
the key does not exist, focus will remain on the currently focused widget.
-}
setFocusOnKey :: WidgetEnv s e -> WidgetKey -> EventResponse s e sp ep
setFocusOnKey wenv key = Request (SetFocus widgetId) where
  widgetId = fromMaybe def (widgetIdFromKey wenv key)

-- | Generates a response that sets the clipboard to the given data
setClipboardData :: ClipboardData -> EventResponse s e sp ep
setClipboardData cdata = Request (SetClipboard cdata)

-- | Generates a response that sets the cursor to the given icon
setCursorIcon :: WidgetNode s e -> CursorIcon -> EventResponse s e sp ep
setCursorIcon node icon = Request (SetCursorIcon widgetId icon) where
  widgetId = node ^. L.info . L.widgetId

-- | Generates a response that resets the cursor icon
resetCursorIcon :: WidgetNode s e -> EventResponse s e sp ep
resetCursorIcon node = Request (ResetCursorIcon widgetId) where
  widgetId = node ^. L.info . L.widgetId

-- | Generates a response that exits the application
exitApplication :: EventResponse s e sp ep
exitApplication = Request (ExitApplication True)

-- | Generates a response that cancels a request to exit the application
cancelExitApplication :: EventResponse s e sp ep
cancelExitApplication = Request (ExitApplication False)

{-|
Returns the provided widget when True, otherwise returns an invisible
placeholder.

Useful for conditionally adding a widget to a list.

@
vstack [
  label "Label 1",
  widgetIf isValid (label "Label 2")
]
@
-}
widgetIf :: Bool -> WidgetNode s e -> WidgetNode s e
widgetIf True node = node
widgetIf False _ = spacer `nodeVisible` False

{-|
Returns the result of applying the function when the provided value is Just,
otherwise returns an invisible placeholder.

Useful for conditionally adding a widget to a list.
-}
widgetMaybe :: Maybe a -> (a -> WidgetNode s e) -> WidgetNode s e
widgetMaybe Nothing _ = spacer `nodeVisible` False
widgetMaybe (Just val) fn = fn val

{-|
Returns the provided style when True, otherwise returns the empty style.

Useful for conditionally setting a style.

@
label "Test"
  \`styleBasic\` [
    textFont "Medium",
    styleIf invalidUser (textColor red)
  ]
@
-}
styleIf :: Bool -> StyleState -> StyleState
styleIf True state = state
styleIf False _ = mempty

{-|
Returns the result of applying the function when the provided value is Just,
otherwise returns the empty style.

Useful for conditionally setting a style.
-}
styleMaybe :: Maybe a -> (a -> StyleState) -> StyleState
styleMaybe Nothing _ = mempty
styleMaybe (Just state) fn = fn state

{-|
Returns the provided configuration value when True, otherwise returns the
default ('mempty') configuration value.

Useful for conditionally setting a configuration value.

@
label_ "Test" [textFont "Medium", configIf showAll multiline]
@
-}
configIf :: Monoid a => Bool -> a -> a
configIf True val = val
configIf False _ = mempty

{-|
Returns the result of applying the function when the provided value is Just,
otherwise returns the default ('mempty') configuration value.

Useful for conditionally setting a configuration value.
-}
configMaybe :: Monoid a => Maybe b -> (b -> a) -> a
configMaybe Nothing _ = mempty
configMaybe (Just val) fn = fn val
