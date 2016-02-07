{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Foreign.C.TypesSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Foreign.C.Types" module.
-}
module Spec.Foreign.C.TypesSpec (main, spec) where

import Foreign.C.Types

import Instances.Foreign.C.Types ()

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "CChar" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CChar -> Bool)
    describe "CSChar" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CSChar -> Bool)
    describe "CUChar" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CUChar -> Bool)
    describe "CShort" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CShort -> Bool)
    describe "CUShort" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CUShort -> Bool)
    describe "CInt" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CInt -> Bool)
    describe "CUInt" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CUInt -> Bool)
    describe "CLong" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CLong -> Bool)
    describe "CULong" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CULong -> Bool)
    describe "CPtrdiff" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CPtrdiff -> Bool)
    describe "CSize" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CSize -> Bool)
    describe "CWchar" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CWchar -> Bool)
    describe "CSigAtomic" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CSigAtomic -> Bool)
    describe "CLLong" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CLLong -> Bool)
    describe "CULLong" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CULLong -> Bool)
    describe "CIntPtr" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CIntPtr -> Bool)
    describe "CUIntPtr" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CUIntPtr -> Bool)
    describe "CIntMax" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CIntMax -> Bool)
    describe "CUIntMax" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CUIntMax -> Bool)
    describe "CClock" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CClock -> Bool)
    describe "CTime" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CTime -> Bool)
#if MIN_VERSION_base(4,4,0)
    describe "CUSeconds" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CUSeconds -> Bool)
    describe "CSUSeconds" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CSUSeconds -> Bool)
#endif
    describe "CFloat" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CFloat -> Bool)
    describe "CDouble" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> CDouble -> Bool)
