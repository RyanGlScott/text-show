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
spec = parallel . describe "Text.Show.Text.Foreign.C.Types" $ do
    prop "CChar instance"      (prop_matchesShow :: Int -> CChar -> Bool)
    prop "CSChar instance"     (prop_matchesShow :: Int -> CSChar -> Bool)
    prop "CUChar instance"     (prop_matchesShow :: Int -> CUChar -> Bool)
    prop "CShort instance"     (prop_matchesShow :: Int -> CShort -> Bool)
    prop "CUShort instance"    (prop_matchesShow :: Int -> CUShort -> Bool)
    prop "CInt instance"       (prop_matchesShow :: Int -> CInt -> Bool)
    prop "CUInt instance"      (prop_matchesShow :: Int -> CUInt -> Bool)
    prop "CLong instance"      (prop_matchesShow :: Int -> CLong -> Bool)
    prop "CULong instance"     (prop_matchesShow :: Int -> CULong -> Bool)
    prop "CPtrdiff instance"   (prop_matchesShow :: Int -> CPtrdiff -> Bool)
    prop "CSize instance"      (prop_matchesShow :: Int -> CSize -> Bool)
    prop "CWchar instance"     (prop_matchesShow :: Int -> CWchar -> Bool)
    prop "CSigAtomic instance" (prop_matchesShow :: Int -> CSigAtomic -> Bool)
    prop "CLLong instance"     (prop_matchesShow :: Int -> CLLong -> Bool)
    prop "CULLong instance"    (prop_matchesShow :: Int -> CULLong -> Bool)
    prop "CIntPtr instance"    (prop_matchesShow :: Int -> CIntPtr -> Bool)
    prop "CUIntPtr instance"   (prop_matchesShow :: Int -> CUIntPtr -> Bool)
    prop "CIntMax instance"    (prop_matchesShow :: Int -> CIntMax -> Bool)
    prop "CUIntMax instance"   (prop_matchesShow :: Int -> CUIntMax -> Bool)
    prop "CClock instance"     (prop_matchesShow :: Int -> CClock -> Bool)
    prop "CTime instance"      (prop_matchesShow :: Int -> CTime -> Bool)
#if MIN_VERSION_base(4,4,0)
    prop "CUSeconds instance"  (prop_matchesShow :: Int -> CUSeconds -> Bool)
    prop "CSUSeconds instance" (prop_matchesShow :: Int -> CSUSeconds -> Bool)
#endif
    prop "CFloat instance"     (prop_matchesShow :: Int -> CFloat -> Bool)
    prop "CDouble instance"    (prop_matchesShow :: Int -> CDouble -> Bool)
