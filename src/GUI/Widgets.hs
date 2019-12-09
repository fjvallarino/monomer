{-# LANGUAGE RecordWildCards #-}

module GUI.Widgets (
  module GUI.Widget.Core,
  module GUI.Widget.Button,
  module GUI.Widget.Layout,
  module GUI.Widget.Label,
  module GUI.Widget.Sandbox,
  module GUI.Widget.Scroll,
  module GUI.Widget.TextField
) where

import GUI.Widget.Core (key, style, children)
import GUI.Widget.Button
import GUI.Widget.Layout
import GUI.Widget.Label
import GUI.Widget.Sandbox
import GUI.Widget.Scroll
import GUI.Widget.TextField
