{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.DefaultAssets.RobotoRegular(robotoRegular, defaultFontDef) where

import Data.ByteString
import Data.FileEmbed
import Monomer.Graphics.Types (FontDef (..))

defaultFontDef :: FontDef
defaultFontDef = FontDefMem "Regular" robotoRegular

robotoRegular :: ByteString
robotoRegular = $(makeRelativeToProject "src/Monomer/DefaultAssets/Roboto-Regular.ttf" >>= embedFile)