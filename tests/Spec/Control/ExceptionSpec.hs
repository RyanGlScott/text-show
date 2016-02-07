{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Control.ExceptionSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Control.Exception" module.
-}
module Spec.Control.ExceptionSpec (main, spec) where

import Control.Exception

import Instances.Control.Exception ()

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "TextShow.Control.Exception" $ do
    describe "SomeException" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> SomeException -> Bool)
    describe "IOException" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> IOException -> Bool)
    describe "ArithException" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ArithException -> Bool)
    describe "ArrayException" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ArrayException -> Bool)
    describe "AssertionFailed" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> AssertionFailed -> Bool)
#if MIN_VERSION_base(4,7,0)
    describe "SomeAsyncException" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> SomeAsyncException -> Bool)
#endif
    describe "AsyncException" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> AsyncException -> Bool)
    describe "NonTermination" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> NonTermination -> Bool)
    describe "NestedAtomically" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> NestedAtomically -> Bool)
    describe "BlockedIndefinitelyOnMVar" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> BlockedIndefinitelyOnMVar -> Bool)
    describe "BlockedIndefinitelyOnSTM" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> BlockedIndefinitelyOnSTM -> Bool)
#if MIN_VERSION_base(4,8,0)
    describe "AllocationLimitExceeded" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> AllocationLimitExceeded -> Bool)
#endif
#if MIN_VERSION_base(4,9,0)
    describe "TypeError" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TypeError -> Bool)
#endif
    describe "Deadlock" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Deadlock -> Bool)
    describe "NoMethodError" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> NoMethodError -> Bool)
    describe "PatternMatchFail" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> PatternMatchFail -> Bool)
    describe "RecConError" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> RecConError -> Bool)
    describe "RecSelError" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> RecSelError -> Bool)
    describe "RecUpdError" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> RecUpdError -> Bool)
    describe "ErrorCall" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> ErrorCall -> Bool)
    describe "MaskingState" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> MaskingState -> Bool)
