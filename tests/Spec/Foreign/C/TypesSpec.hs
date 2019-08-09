{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Foreign.C.TypesSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Foreign.C.Types" module.
-}
module Spec.Foreign.C.TypesSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Foreign.C.Types
import Instances.Foreign.C.Types ()
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "CChar" $
        matchesTextShowSpec (Proxy :: Proxy CChar)
    describe "CSChar" $
        matchesTextShowSpec (Proxy :: Proxy CSChar)
    describe "CUChar" $
        matchesTextShowSpec (Proxy :: Proxy CUChar)
    describe "CShort" $
        matchesTextShowSpec (Proxy :: Proxy CShort)
    describe "CUShort" $
        matchesTextShowSpec (Proxy :: Proxy CUShort)
    describe "CInt" $
        matchesTextShowSpec (Proxy :: Proxy CInt)
    describe "CUInt" $
        matchesTextShowSpec (Proxy :: Proxy CUInt)
    describe "CLong" $
        matchesTextShowSpec (Proxy :: Proxy CLong)
    describe "CULong" $
        matchesTextShowSpec (Proxy :: Proxy CULong)
    describe "CPtrdiff" $
        matchesTextShowSpec (Proxy :: Proxy CPtrdiff)
    describe "CSize" $
        matchesTextShowSpec (Proxy :: Proxy CSize)
    describe "CWchar" $
        matchesTextShowSpec (Proxy :: Proxy CWchar)
    describe "CSigAtomic" $
        matchesTextShowSpec (Proxy :: Proxy CSigAtomic)
    describe "CLLong" $
        matchesTextShowSpec (Proxy :: Proxy CLLong)
    describe "CULLong" $
        matchesTextShowSpec (Proxy :: Proxy CULLong)
    describe "CIntPtr" $
        matchesTextShowSpec (Proxy :: Proxy CIntPtr)
    describe "CUIntPtr" $
        matchesTextShowSpec (Proxy :: Proxy CUIntPtr)
    describe "CIntMax" $
        matchesTextShowSpec (Proxy :: Proxy CIntMax)
    describe "CUIntMax" $
        matchesTextShowSpec (Proxy :: Proxy CUIntMax)
    describe "CClock" $
        matchesTextShowSpec (Proxy :: Proxy CClock)
    describe "CTime" $
        matchesTextShowSpec (Proxy :: Proxy CTime)
    describe "CUSeconds" $
        matchesTextShowSpec (Proxy :: Proxy CUSeconds)
    describe "CSUSeconds" $
        matchesTextShowSpec (Proxy :: Proxy CSUSeconds)
    describe "CFloat" $
        matchesTextShowSpec (Proxy :: Proxy CFloat)
    describe "CDouble" $
        matchesTextShowSpec (Proxy :: Proxy CDouble)
#if MIN_VERSION_base(4,10,0)
    describe "CBool" $
        matchesTextShowSpec (Proxy :: Proxy CBool)
#endif
