{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow

import qualified Tutorial00
import qualified Tutorial01
import qualified Tutorial02
import qualified Tutorial03

main :: IO ()
main = do
--  Tutorial00.main00
--  Tutorial01.main01
--  Tutorial02.main02
  Tutorial03.main03
