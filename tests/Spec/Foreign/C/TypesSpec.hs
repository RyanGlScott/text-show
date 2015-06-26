{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Foreign.C.TypesSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Foreign.C.Types" module.
-}
module Spec.Foreign.C.TypesSpec (main, spec) where

import Foreign.C.Types

import Instances.Foreign.C.Types ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "CChar" $
        prop "Show instance" (prop_matchesShow :: Int -> CChar -> Bool)
    describe "CSChar" $
        prop "Show instance" (prop_matchesShow :: Int -> CSChar -> Bool)
    describe "CUChar" $
        prop "Show instance" (prop_matchesShow :: Int -> CUChar -> Bool)
    describe "CShort" $
        prop "Show instance" (prop_matchesShow :: Int -> CShort -> Bool)
    describe "CUShort" $
        prop "Show instance" (prop_matchesShow :: Int -> CUShort -> Bool)
    describe "CInt" $
        prop "Show instance" (prop_matchesShow :: Int -> CInt -> Bool)
    describe "CUInt" $
        prop "Show instance" (prop_matchesShow :: Int -> CUInt -> Bool)
    describe "CLong" $
        prop "Show instance" (prop_matchesShow :: Int -> CLong -> Bool)
    describe "CULong" $
        prop "Show instance" (prop_matchesShow :: Int -> CULong -> Bool)
    describe "CPtrdiff" $
        prop "Show instance" (prop_matchesShow :: Int -> CPtrdiff -> Bool)
    describe "CSize" $
        prop "Show instance" (prop_matchesShow :: Int -> CSize -> Bool)
    describe "CWchar" $
        prop "Show instance" (prop_matchesShow :: Int -> CWchar -> Bool)
    describe "CSigAtomic" $
        prop "Show instance" (prop_matchesShow :: Int -> CSigAtomic -> Bool)
    describe "CLLong" $
        prop "Show instance" (prop_matchesShow :: Int -> CLLong -> Bool)
    describe "CULLong" $
        prop "Show instance" (prop_matchesShow :: Int -> CULLong -> Bool)
    describe "CIntPtr" $
        prop "Show instance" (prop_matchesShow :: Int -> CIntPtr -> Bool)
    describe "CUIntPtr" $
        prop "Show instance" (prop_matchesShow :: Int -> CUIntPtr -> Bool)
    describe "CIntMax" $
        prop "Show instance" (prop_matchesShow :: Int -> CIntMax -> Bool)
    describe "CUIntMax" $
        prop "Show instance" (prop_matchesShow :: Int -> CUIntMax -> Bool)
    describe "CClock" $
        prop "Show instance" (prop_matchesShow :: Int -> CClock -> Bool)
    describe "CTime" $
        prop "Show instance" (prop_matchesShow :: Int -> CTime -> Bool)
#if MIN_VERSION_base(4,4,0)
    describe "CUSeconds" $
        prop "Show instance" (prop_matchesShow :: Int -> CUSeconds -> Bool)
    describe "CSUSeconds" $
        prop "Show instance" (prop_matchesShow :: Int -> CSUSeconds -> Bool)
#endif
    describe "CFloat" $
        prop "Show instance" (prop_matchesShow :: Int -> CFloat -> Bool)
    describe "CDouble" $
        prop "Show instance" (prop_matchesShow :: Int -> CDouble -> Bool)
