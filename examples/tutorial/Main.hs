{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow

import qualified Tutorial01
import qualified Tutorial02
import qualified Tutorial03
import qualified Tutorial04
import qualified Tutorial05

main :: IO ()
main = do
--  Tutorial01.main01
--  Tutorial02.main02
--  Tutorial03.main03
--  Tutorial04.main04
  Tutorial05.main05
