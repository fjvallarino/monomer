{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow

import qualified Tutorial01_Basics
import qualified Tutorial02_Styling
import qualified Tutorial03_LifeCycle
import qualified Tutorial04_Tasks
import qualified Tutorial05_Producers
import qualified Tutorial06_Composite
import qualified Tutorial07_CustomWidget

main :: IO ()
main = do
--  Tutorial01_Basics.main01
--  Tutorial02_Styling.main02
--  Tutorial03_LifeCycle.main03
--  Tutorial04_Tasks.main04
--  Tutorial05_Producers.main05
--  Tutorial06_Composite.main06
  Tutorial07_CustomWidget.main07
