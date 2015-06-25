{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Control.ExceptionSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Control.Exception" module.
-}
module Spec.Control.ExceptionSpec (main, spec) where

import Control.Exception

import Instances.Control.Exception ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Control.Exception" $ do
    prop "SomeException instance"             (prop_matchesShow :: Int -> SomeException -> Bool)
    prop "IOException instance"               (prop_matchesShow :: Int -> IOException -> Bool)
    prop "ArithException instance"            (prop_matchesShow :: Int -> ArithException -> Bool)
    prop "ArrayException instance"            (prop_matchesShow :: Int -> ArrayException -> Bool)
    prop "AssertionFailed instance"           (prop_matchesShow :: Int -> AssertionFailed -> Bool)
#if MIN_VERSION_base(4,7,0)
    prop "SomeAsyncException instance"        (prop_matchesShow :: Int -> SomeAsyncException -> Bool)
#endif
    prop "AsyncException instance"            (prop_matchesShow :: Int -> AsyncException -> Bool)
    prop "NonTermination instance"            (prop_matchesShow :: Int -> NonTermination -> Bool)
    prop "NestedAtomically instance"          (prop_matchesShow :: Int -> NestedAtomically -> Bool)
    prop "BlockedIndefinitelyOnMVar instance" (prop_matchesShow :: Int -> BlockedIndefinitelyOnMVar -> Bool)
    prop "BlockedIndefinitelyOnSTM instance"  (prop_matchesShow :: Int -> BlockedIndefinitelyOnSTM -> Bool)
#if MIN_VERSION_base(4,8,0)
    prop "AllocationLimitExceeded instance"   (prop_matchesShow :: Int -> AllocationLimitExceeded -> Bool)
#endif
    prop "Deadlock instance"                  (prop_matchesShow :: Int -> Deadlock -> Bool)
    prop "NoMethodError instance"             (prop_matchesShow :: Int -> NoMethodError -> Bool)
    prop "PatternMatchFail instance"          (prop_matchesShow :: Int -> PatternMatchFail -> Bool)
    prop "RecConError instance"               (prop_matchesShow :: Int -> RecConError -> Bool)
    prop "RecSelError instance"               (prop_matchesShow :: Int -> RecSelError -> Bool)
    prop "RecUpdError instance"               (prop_matchesShow :: Int -> RecUpdError -> Bool)
    prop "ErrorCall instance"                 (prop_matchesShow :: Int -> ErrorCall -> Bool)
    prop "MaskingState instance"              (prop_matchesShow :: Int -> MaskingState -> Bool)
