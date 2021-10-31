{-|
Module      : Main
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Main module for the tutorials.
-}
module Main where

import qualified Tutorial01_Basics
import qualified Tutorial02_Styling
import qualified Tutorial03_LifeCycle
import qualified Tutorial04_Tasks
import qualified Tutorial05_Producers
import qualified Tutorial06_Composite
import qualified Tutorial07_CustomWidget
import qualified Tutorial08_Themes

main :: IO ()
main = do
--  Tutorial01_Basics.main01
--  Tutorial02_Styling.main02
--  Tutorial03_LifeCycle.main03
--  Tutorial04_Tasks.main04
--  Tutorial05_Producers.main05
  Tutorial06_Composite.main06
--  Tutorial07_CustomWidget.main07
--  Tutorial08_Themes.main08
