{-|
Module      : Monomer.Event.Keyboard
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Keycodes for all keyboard keys.
-}
module Monomer.Event.Keyboard where

import qualified SDL

import Monomer.Event.Types

getKeyCode :: SDL.Keycode -> KeyCode
getKeyCode keyCode = KeyCode $ fromIntegral (SDL.unwrapKeycode keyCode)

-- Mod Keys
keyLAlt :: KeyCode
keyLAlt = getKeyCode SDL.KeycodeLAlt

keyRAlt :: KeyCode
keyRAlt = getKeyCode SDL.KeycodeLAlt

keyLCtrl :: KeyCode
keyLCtrl = getKeyCode SDL.KeycodeLCtrl

keyRCtrl :: KeyCode
keyRCtrl = getKeyCode SDL.KeycodeLCtrl

keyLGUI :: KeyCode
keyLGUI = getKeyCode SDL.KeycodeLGUI

keyRGUI :: KeyCode
keyRGUI = getKeyCode SDL.KeycodeLGUI

keyLShift :: KeyCode
keyLShift = getKeyCode SDL.KeycodeLShift

keyRShift :: KeyCode
keyRShift = getKeyCode SDL.KeycodeLShift

-- General keys
keyUnknown :: KeyCode
keyUnknown = getKeyCode SDL.KeycodeUnknown

keyReturn :: KeyCode
keyReturn = getKeyCode SDL.KeycodeReturn

keyEscape :: KeyCode
keyEscape = getKeyCode SDL.KeycodeEscape

keyBackspace :: KeyCode
keyBackspace = getKeyCode SDL.KeycodeBackspace

keyTab :: KeyCode
keyTab = getKeyCode SDL.KeycodeTab

keySpace :: KeyCode
keySpace = getKeyCode SDL.KeycodeSpace

keyExclaim :: KeyCode
keyExclaim = getKeyCode SDL.KeycodeExclaim

keyQuoteDbl :: KeyCode
keyQuoteDbl = getKeyCode SDL.KeycodeQuoteDbl

keyHash :: KeyCode
keyHash = getKeyCode SDL.KeycodeHash

keyPercent :: KeyCode
keyPercent = getKeyCode SDL.KeycodePercent

keyDollar :: KeyCode
keyDollar = getKeyCode SDL.KeycodeDollar

keyAmpersand :: KeyCode
keyAmpersand = getKeyCode SDL.KeycodeAmpersand

keyQuote :: KeyCode
keyQuote = getKeyCode SDL.KeycodeQuote

keyLeftParen :: KeyCode
keyLeftParen = getKeyCode SDL.KeycodeLeftParen

keyRightParen :: KeyCode
keyRightParen = getKeyCode SDL.KeycodeRightParen

keyAsterisk :: KeyCode
keyAsterisk = getKeyCode SDL.KeycodeAsterisk

keyPlus :: KeyCode
keyPlus = getKeyCode SDL.KeycodePlus

keyComma :: KeyCode
keyComma = getKeyCode SDL.KeycodeComma

keyMinus :: KeyCode
keyMinus = getKeyCode SDL.KeycodeMinus

keyPeriod :: KeyCode
keyPeriod = getKeyCode SDL.KeycodePeriod

keySlash :: KeyCode
keySlash = getKeyCode SDL.KeycodeSlash

keyColon :: KeyCode
keyColon = getKeyCode SDL.KeycodeColon

keySemicolon :: KeyCode
keySemicolon = getKeyCode SDL.KeycodeSemicolon

keyLess :: KeyCode
keyLess = getKeyCode SDL.KeycodeLess

keyEquals :: KeyCode
keyEquals = getKeyCode SDL.KeycodeEquals

keyGreater :: KeyCode
keyGreater = getKeyCode SDL.KeycodeGreater

keyQuestion :: KeyCode
keyQuestion = getKeyCode SDL.KeycodeQuestion

keyAt :: KeyCode
keyAt = getKeyCode SDL.KeycodeAt

keyLeftBracket :: KeyCode
keyLeftBracket = getKeyCode SDL.KeycodeLeftBracket

keyBackslash :: KeyCode
keyBackslash = getKeyCode SDL.KeycodeBackslash

keyRightBracket :: KeyCode
keyRightBracket = getKeyCode SDL.KeycodeRightBracket

keyCaret :: KeyCode
keyCaret = getKeyCode SDL.KeycodeCaret

keyUnderscore :: KeyCode
keyUnderscore = getKeyCode SDL.KeycodeUnderscore

keyBackquote :: KeyCode
keyBackquote = getKeyCode SDL.KeycodeBackquote

keyCapsLock :: KeyCode
keyCapsLock = getKeyCode SDL.KeycodeCapsLock

keyPrintScreen :: KeyCode
keyPrintScreen = getKeyCode SDL.KeycodePrintScreen

keyScrollLock :: KeyCode
keyScrollLock = getKeyCode SDL.KeycodeScrollLock

keyPause :: KeyCode
keyPause = getKeyCode SDL.KeycodePause

keyInsert :: KeyCode
keyInsert = getKeyCode SDL.KeycodeInsert

keyHome :: KeyCode
keyHome = getKeyCode SDL.KeycodeHome

keyPageUp :: KeyCode
keyPageUp = getKeyCode SDL.KeycodePageUp

keyDelete :: KeyCode
keyDelete = getKeyCode SDL.KeycodeDelete

keyEnd :: KeyCode
keyEnd = getKeyCode SDL.KeycodeEnd

keyPageDown :: KeyCode
keyPageDown = getKeyCode SDL.KeycodePageDown

keyRight :: KeyCode
keyRight = getKeyCode SDL.KeycodeRight

keyLeft :: KeyCode
keyLeft = getKeyCode SDL.KeycodeLeft

keyDown :: KeyCode
keyDown = getKeyCode SDL.KeycodeDown

keyUp :: KeyCode
keyUp = getKeyCode SDL.KeycodeUp

keyNumLockClear :: KeyCode
keyNumLockClear = getKeyCode SDL.KeycodeNumLockClear

-- Numbers
key0 :: KeyCode
key0 = getKeyCode SDL.Keycode0

key1 :: KeyCode
key1 = getKeyCode SDL.Keycode1

key2 :: KeyCode
key2 = getKeyCode SDL.Keycode2

key3 :: KeyCode
key3 = getKeyCode SDL.Keycode3

key4 :: KeyCode
key4 = getKeyCode SDL.Keycode4

key5 :: KeyCode
key5 = getKeyCode SDL.Keycode5

key6 :: KeyCode
key6 = getKeyCode SDL.Keycode6

key7 :: KeyCode
key7 = getKeyCode SDL.Keycode7

key8 :: KeyCode
key8 = getKeyCode SDL.Keycode8

key9 :: KeyCode
key9 = getKeyCode SDL.Keycode9

-- Function keys
keyF1 :: KeyCode
keyF1 =  getKeyCode SDL.KeycodeF1

keyF2 :: KeyCode
keyF2 =  getKeyCode SDL.KeycodeF2

keyF3 :: KeyCode
keyF3 =  getKeyCode SDL.KeycodeF3

keyF4 :: KeyCode
keyF4 =  getKeyCode SDL.KeycodeF4

keyF5 :: KeyCode
keyF5 =  getKeyCode SDL.KeycodeF5

keyF6 :: KeyCode
keyF6 =  getKeyCode SDL.KeycodeF6

keyF7 :: KeyCode
keyF7 =  getKeyCode SDL.KeycodeF7

keyF8 :: KeyCode
keyF8 =  getKeyCode SDL.KeycodeF8

keyF9 :: KeyCode
keyF9 =  getKeyCode SDL.KeycodeF9

keyF10 :: KeyCode
keyF10 =  getKeyCode SDL.KeycodeF10

keyF11 :: KeyCode
keyF11 =  getKeyCode SDL.KeycodeF11

keyF12 :: KeyCode
keyF12 =  getKeyCode SDL.KeycodeF12

-- Letters
keyA :: KeyCode
keyA = getKeyCode SDL.KeycodeA

keyB :: KeyCode
keyB = getKeyCode SDL.KeycodeB

keyC :: KeyCode
keyC = getKeyCode SDL.KeycodeC

keyD :: KeyCode
keyD = getKeyCode SDL.KeycodeD

keyE :: KeyCode
keyE = getKeyCode SDL.KeycodeE

keyF :: KeyCode
keyF = getKeyCode SDL.KeycodeF

keyG :: KeyCode
keyG = getKeyCode SDL.KeycodeG

keyH :: KeyCode
keyH = getKeyCode SDL.KeycodeH

keyI :: KeyCode
keyI = getKeyCode SDL.KeycodeI

keyJ :: KeyCode
keyJ = getKeyCode SDL.KeycodeJ

keyK :: KeyCode
keyK = getKeyCode SDL.KeycodeK

keyL :: KeyCode
keyL = getKeyCode SDL.KeycodeL

keyM :: KeyCode
keyM = getKeyCode SDL.KeycodeM

keyN :: KeyCode
keyN = getKeyCode SDL.KeycodeN

keyO :: KeyCode
keyO = getKeyCode SDL.KeycodeO

keyP :: KeyCode
keyP = getKeyCode SDL.KeycodeP

keyQ :: KeyCode
keyQ = getKeyCode SDL.KeycodeQ

keyR :: KeyCode
keyR = getKeyCode SDL.KeycodeR

keyS :: KeyCode
keyS = getKeyCode SDL.KeycodeS

keyT :: KeyCode
keyT = getKeyCode SDL.KeycodeT

keyU :: KeyCode
keyU = getKeyCode SDL.KeycodeU

keyV :: KeyCode
keyV = getKeyCode SDL.KeycodeV

keyW :: KeyCode
keyW = getKeyCode SDL.KeycodeW

keyX :: KeyCode
keyX = getKeyCode SDL.KeycodeX

keyY :: KeyCode
keyY = getKeyCode SDL.KeycodeY

keyZ :: KeyCode
keyZ = getKeyCode SDL.KeycodeZ

--

-- Mod keys
isKeyLAlt :: KeyCode -> Bool
isKeyLAlt = (== keyLAlt)

isKeyRAlt :: KeyCode -> Bool
isKeyRAlt = (== keyRAlt)

isKeyLCtrl :: KeyCode -> Bool
isKeyLCtrl = (== keyLCtrl)

isKeyRCtrl :: KeyCode -> Bool
isKeyRCtrl = (== keyRCtrl)

isKeyLGUI :: KeyCode -> Bool
isKeyLGUI = (== keyLGUI)

isKeyRGUI :: KeyCode -> Bool
isKeyRGUI = (== keyRGUI)

isKeyLShift :: KeyCode -> Bool
isKeyLShift = (== keyLShift)

isKeyRShift :: KeyCode -> Bool
isKeyRShift = (== keyRShift)

-- General keys
isKeyUnknown :: KeyCode -> Bool
isKeyUnknown = (== keyUnknown)

isKeyReturn :: KeyCode -> Bool
isKeyReturn = (== keyReturn)

isKeyEscape :: KeyCode -> Bool
isKeyEscape = (== keyEscape)

isKeyBackspace :: KeyCode -> Bool
isKeyBackspace = (== keyBackspace)

isKeyTab :: KeyCode -> Bool
isKeyTab = (== keyTab)

isKeySpace :: KeyCode -> Bool
isKeySpace = (== keySpace)

isKeyExclaim :: KeyCode -> Bool
isKeyExclaim = (== keyExclaim)

isKeyQuoteDbl :: KeyCode -> Bool
isKeyQuoteDbl = (== keyQuoteDbl)

isKeyHash :: KeyCode -> Bool
isKeyHash = (== keyHash)

isKeyPercent :: KeyCode -> Bool
isKeyPercent = (== keyPercent)

isKeyDollar :: KeyCode -> Bool
isKeyDollar = (== keyDollar)

isKeyAmpersand :: KeyCode -> Bool
isKeyAmpersand = (== keyAmpersand)

isKeyQuote :: KeyCode -> Bool
isKeyQuote = (== keyQuote)

isKeyLeftParen :: KeyCode -> Bool
isKeyLeftParen = (== keyLeftParen)

isKeyRightParen :: KeyCode -> Bool
isKeyRightParen = (== keyRightParen)

isKeyAsterisk :: KeyCode -> Bool
isKeyAsterisk = (== keyAsterisk)

isKeyPlus :: KeyCode -> Bool
isKeyPlus = (== keyPlus)

isKeyComma :: KeyCode -> Bool
isKeyComma = (== keyComma)

isKeyMinus :: KeyCode -> Bool
isKeyMinus = (== keyMinus)

isKeyPeriod :: KeyCode -> Bool
isKeyPeriod = (== keyPeriod)

isKeySlash :: KeyCode -> Bool
isKeySlash = (== keySlash)

isKeyColon :: KeyCode -> Bool
isKeyColon = (== keyColon)

isKeySemicolon :: KeyCode -> Bool
isKeySemicolon = (== keySemicolon)

isKeyLess :: KeyCode -> Bool
isKeyLess = (== keyLess)

isKeyEquals :: KeyCode -> Bool
isKeyEquals = (== keyEquals)

isKeyGreater :: KeyCode -> Bool
isKeyGreater = (== keyGreater)

isKeyQuestion :: KeyCode -> Bool
isKeyQuestion = (== keyQuestion)

isKeyAt :: KeyCode -> Bool
isKeyAt = (== keyAt)

isKeyLeftBracket :: KeyCode -> Bool
isKeyLeftBracket = (== keyLeftBracket)

isKeyBackslash :: KeyCode -> Bool
isKeyBackslash = (== keyBackslash)

isKeyRightBracket :: KeyCode -> Bool
isKeyRightBracket = (== keyRightBracket)

isKeyCaret :: KeyCode -> Bool
isKeyCaret = (== keyCaret)

isKeyUnderscore :: KeyCode -> Bool
isKeyUnderscore = (== keyUnderscore)

isKeyBackquote :: KeyCode -> Bool
isKeyBackquote = (== keyBackquote)

isKeyCapsLock :: KeyCode -> Bool
isKeyCapsLock = (== keyCapsLock)

isKeyPrintScreen :: KeyCode -> Bool
isKeyPrintScreen = (== keyPrintScreen)

isKeyScrollLock :: KeyCode -> Bool
isKeyScrollLock = (== keyScrollLock)

isKeyPause :: KeyCode -> Bool
isKeyPause = (== keyPause)

isKeyInsert :: KeyCode -> Bool
isKeyInsert = (== keyInsert)

isKeyHome :: KeyCode -> Bool
isKeyHome = (== keyHome)

isKeyPageUp :: KeyCode -> Bool
isKeyPageUp = (== keyPageUp)

isKeyDelete :: KeyCode -> Bool
isKeyDelete = (== keyDelete)

isKeyEnd :: KeyCode -> Bool
isKeyEnd = (== keyEnd)

isKeyPageDown :: KeyCode -> Bool
isKeyPageDown = (== keyPageDown)

isKeyRight :: KeyCode -> Bool
isKeyRight = (== keyRight)

isKeyLeft :: KeyCode -> Bool
isKeyLeft = (== keyLeft)

isKeyDown :: KeyCode -> Bool
isKeyDown = (== keyDown)

isKeyUp :: KeyCode -> Bool
isKeyUp = (== keyUp)

isKeyNumLockClear :: KeyCode -> Bool
isKeyNumLockClear = (== keyNumLockClear)

-- Numbers
isKey0 :: KeyCode -> Bool
isKey0 = (== key0)

isKey1 :: KeyCode -> Bool
isKey1 = (== key1)

isKey2 :: KeyCode -> Bool
isKey2 = (== key2)

isKey3 :: KeyCode -> Bool
isKey3 = (== key3)

isKey4 :: KeyCode -> Bool
isKey4 = (== key4)

isKey5 :: KeyCode -> Bool
isKey5 = (== key5)

isKey6 :: KeyCode -> Bool
isKey6 = (== key6)

isKey7 :: KeyCode -> Bool
isKey7 = (== key7)

isKey8 :: KeyCode -> Bool
isKey8 = (== key8)

isKey9 :: KeyCode -> Bool
isKey9 = (== key9)

isKeyF1 :: KeyCode -> Bool
isKeyF1 = (== keyF1)

isKeyF2 :: KeyCode -> Bool
isKeyF2 = (== keyF2)

isKeyF3 :: KeyCode -> Bool
isKeyF3 = (== keyF3)

isKeyF4 :: KeyCode -> Bool
isKeyF4 = (== keyF4)

isKeyF5 :: KeyCode -> Bool
isKeyF5 = (== keyF5)

isKeyF6 :: KeyCode -> Bool
isKeyF6 = (== keyF6)

isKeyF7 :: KeyCode -> Bool
isKeyF7 = (== keyF7)

isKeyF8 :: KeyCode -> Bool
isKeyF8 = (== keyF8)

isKeyF9 :: KeyCode -> Bool
isKeyF9 = (== keyF9)

isKeyF10 :: KeyCode -> Bool
isKeyF10 = (== keyF10)

isKeyF11 :: KeyCode -> Bool
isKeyF11 = (== keyF11)

isKeyF12 :: KeyCode -> Bool
isKeyF12 = (== keyF12)

-- Letters
isKeyA :: KeyCode -> Bool
isKeyA = (== keyA)

isKeyB :: KeyCode -> Bool
isKeyB = (== keyB)

isKeyC :: KeyCode -> Bool
isKeyC = (== keyC)

isKeyD :: KeyCode -> Bool
isKeyD = (== keyD)

isKeyE :: KeyCode -> Bool
isKeyE = (== keyE)

isKeyF :: KeyCode -> Bool
isKeyF = (== keyF)

isKeyG :: KeyCode -> Bool
isKeyG = (== keyG)

isKeyH :: KeyCode -> Bool
isKeyH = (== keyH)

isKeyI :: KeyCode -> Bool
isKeyI = (== keyI)

isKeyJ :: KeyCode -> Bool
isKeyJ = (== keyJ)

isKeyK :: KeyCode -> Bool
isKeyK = (== keyK)

isKeyL :: KeyCode -> Bool
isKeyL = (== keyL)

isKeyM :: KeyCode -> Bool
isKeyM = (== keyM)

isKeyN :: KeyCode -> Bool
isKeyN = (== keyN)

isKeyO :: KeyCode -> Bool
isKeyO = (== keyO)

isKeyP :: KeyCode -> Bool
isKeyP = (== keyP)

isKeyQ :: KeyCode -> Bool
isKeyQ = (== keyQ)

isKeyR :: KeyCode -> Bool
isKeyR = (== keyR)

isKeyS :: KeyCode -> Bool
isKeyS = (== keyS)

isKeyT :: KeyCode -> Bool
isKeyT = (== keyT)

isKeyU :: KeyCode -> Bool
isKeyU = (== keyU)

isKeyV :: KeyCode -> Bool
isKeyV = (== keyV)

isKeyW :: KeyCode -> Bool
isKeyW = (== keyW)

isKeyX :: KeyCode -> Bool
isKeyX = (== keyX)

isKeyY :: KeyCode -> Bool
isKeyY = (== keyY)

isKeyZ :: KeyCode -> Bool
isKeyZ = (== keyZ)
