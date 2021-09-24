{-|
Module      : External01_Lenses
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Main module for the 'Lenses' example.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module External01_Lenses where

import Control.Lens
import Data.Maybe
import Data.Text (Text)

data Address = Address {
  _street :: Text,
  _doorNumber :: Int
} deriving (Eq, Show)

data Person = Person {
  _name :: Text,
  _homeAddress :: Address,
  _workAddress :: Maybe Address,
  _friends :: [Person]
} deriving (Eq, Show)

makeLenses ''Address
makeLenses ''Person

main :: IO ()
main = do
  putStrLn "Basic get:\n"
  print address1
  print $ address1 ^. street
  print person1
  print $ person1 ^. homeAddress
  print $ person1 ^. homeAddress . doorNumber

  putStrLn "\nBasic update:\n"
  print $ person1
    & homeAddress . street .~ "Road 3"
    & homeAddress . doorNumber .~ 777

  putStrLn "\nPrism get:\n"
  print $ person1 ^. workAddress
  -- print $ person1 ^? workAddress . doorNumber -- Does not compile
  print $ person1 ^? workAddress . _Just . doorNumber

  putStrLn "\nPrism update:\n"
  print person2
  print $ person2
    & workAddress .~ Just address1
  print $ person2
    & workAddress ?~ address1

  putStrLn "\nIndexed get:\n"
  print $ person2 ^? friends . ix 0 . homeAddress
  print $ person2 ^?! friends . ix 0 . homeAddress
  print $ person2 ^. friends . singular (ix 0) . homeAddress

  putStrLn "\nIndexed update:\n"
  print $ person2
    & friends . ix 0 .~ person3
  print $ person2
    & friends . ix 10 .~ person3
  where
    address1 = Address "Street 1" 1234
    address2 = Address "Avenue 3" 987
    address3 = Address "Boulevard 9" 4756

    person1 = Person "Mark" address1 Nothing []
    person2 = Person "Jane" address2 (Just address3) [person1]
    person3 = Person "Zack" address3 Nothing []
