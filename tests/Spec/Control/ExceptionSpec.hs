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
import Data.Proxy (Proxy(..))
import Instances.Control.Exception ()
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "TextShow.Control.Exception" $ do
    describe "SomeException" $
        matchesTextShowSpec (Proxy :: Proxy SomeException)
    describe "IOException" $
        matchesTextShowSpec (Proxy :: Proxy IOException)
    describe "ArithException" $
        matchesTextShowSpec (Proxy :: Proxy ArithException)
    describe "ArrayException" $
        matchesTextShowSpec (Proxy :: Proxy ArrayException)
    describe "AssertionFailed" $
        matchesTextShowSpec (Proxy :: Proxy AssertionFailed)
#if MIN_VERSION_base(4,7,0)
    describe "SomeAsyncException" $
        matchesTextShowSpec (Proxy :: Proxy SomeAsyncException)
#endif
    describe "AsyncException" $
        matchesTextShowSpec (Proxy :: Proxy AsyncException)
    describe "NonTermination" $
        matchesTextShowSpec (Proxy :: Proxy NonTermination)
    describe "NestedAtomically" $
        matchesTextShowSpec (Proxy :: Proxy NestedAtomically)
    describe "BlockedIndefinitelyOnMVar" $
        matchesTextShowSpec (Proxy :: Proxy BlockedIndefinitelyOnMVar)
    describe "BlockedIndefinitelyOnSTM" $
        matchesTextShowSpec (Proxy :: Proxy BlockedIndefinitelyOnSTM)
#if MIN_VERSION_base(4,8,0)
    describe "AllocationLimitExceeded" $
        matchesTextShowSpec (Proxy :: Proxy AllocationLimitExceeded)
#endif
#if MIN_VERSION_base(4,9,0)
    describe "TypeError" $
        matchesTextShowSpec (Proxy :: Proxy TypeError)
#endif
    describe "Deadlock" $
        matchesTextShowSpec (Proxy :: Proxy Deadlock)
    describe "NoMethodError" $
        matchesTextShowSpec (Proxy :: Proxy NoMethodError)
    describe "PatternMatchFail" $
        matchesTextShowSpec (Proxy :: Proxy PatternMatchFail)
    describe "RecConError" $
        matchesTextShowSpec (Proxy :: Proxy RecConError)
    describe "RecSelError" $
        matchesTextShowSpec (Proxy :: Proxy RecSelError)
    describe "RecUpdError" $
        matchesTextShowSpec (Proxy :: Proxy RecUpdError)
    describe "ErrorCall" $
        matchesTextShowSpec (Proxy :: Proxy ErrorCall)
    describe "MaskingState" $
        matchesTextShowSpec (Proxy :: Proxy MaskingState)
