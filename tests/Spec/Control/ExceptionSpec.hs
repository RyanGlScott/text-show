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
    describe "SomeException" $
        prop "Show instance" (prop_matchesShow :: Int -> SomeException -> Bool)
    describe "IOException" $
        prop "Show instance" (prop_matchesShow :: Int -> IOException -> Bool)
    describe "ArithException" $
        prop "Show instance" (prop_matchesShow :: Int -> ArithException -> Bool)
    describe "ArrayException" $
        prop "Show instance" (prop_matchesShow :: Int -> ArrayException -> Bool)
    describe "AssertionFailed" $
        prop "Show instance" (prop_matchesShow :: Int -> AssertionFailed -> Bool)
#if MIN_VERSION_base(4,7,0)
    describe "SomeAsyncException" $
        prop "Show instance" (prop_matchesShow :: Int -> SomeAsyncException -> Bool)
#endif
    describe "AsyncException" $
        prop "Show instance" (prop_matchesShow :: Int -> AsyncException -> Bool)
    describe "NonTermination" $
        prop "Show instance" (prop_matchesShow :: Int -> NonTermination -> Bool)
    describe "NestedAtomically" $
        prop "Show instance" (prop_matchesShow :: Int -> NestedAtomically -> Bool)
    describe "BlockedIndefinitelyOnMVar" $
        prop "Show instance" (prop_matchesShow :: Int -> BlockedIndefinitelyOnMVar -> Bool)
    describe "BlockedIndefinitelyOnSTM" $
        prop "Show instance" (prop_matchesShow :: Int -> BlockedIndefinitelyOnSTM -> Bool)
#if MIN_VERSION_base(4,8,0)
    describe "AllocationLimitExceeded" $
        prop "Show instance" (prop_matchesShow :: Int -> AllocationLimitExceeded -> Bool)
#endif
    describe "Deadlock" $
        prop "Show instance" (prop_matchesShow :: Int -> Deadlock -> Bool)
    describe "NoMethodError" $
        prop "Show instance" (prop_matchesShow :: Int -> NoMethodError -> Bool)
    describe "PatternMatchFail" $
        prop "Show instance" (prop_matchesShow :: Int -> PatternMatchFail -> Bool)
    describe "RecConError" $
        prop "Show instance" (prop_matchesShow :: Int -> RecConError -> Bool)
    describe "RecSelError" $
        prop "Show instance" (prop_matchesShow :: Int -> RecSelError -> Bool)
    describe "RecUpdError" $
        prop "Show instance" (prop_matchesShow :: Int -> RecUpdError -> Bool)
    describe "ErrorCall" $
        prop "Show instance" (prop_matchesShow :: Int -> ErrorCall -> Bool)
    describe "MaskingState" $
        prop "Show instance" (prop_matchesShow :: Int -> MaskingState -> Bool)
