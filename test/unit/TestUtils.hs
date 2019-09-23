module TestUtils where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.CallStack
import Hedgehog
import Hedgehog.Classes
import Test.HUnit.Lang
import System.IO.Silently

import qualified Control.Exception as E

{-- Adapted from: http://hackage.haskell.org/package/hw-hspec-hedgehog --}
location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  []           -> Nothing

require :: HasCallStack => Property -> Assertion
require p = do
  (captured, result) <- capture $ liftIO $ check p

  unless result $ do
    putStrLn captured
    E.throwIO (HUnitFailure location $ Reason "Hedgehog property test failed")

checkLaws :: HasCallStack => Gen a -> [Gen a -> Laws] -> Assertion
checkLaws gen laws = do
  (captured, result) <- capture $ lawsCheckOne gen laws

  unless result $ do
    putStrLn captured
    E.throwIO (HUnitFailure location $ Reason "Hedgehog classes property test failed")
