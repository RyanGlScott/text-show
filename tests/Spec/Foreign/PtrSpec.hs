{-|
Module:      Spec.Foreign.PtrSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for pointer data types.
-}
module Spec.Foreign.PtrSpec (main, spec) where

import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (FunPtr, IntPtr, Ptr, WordPtr)

import Instances.Foreign.Ptr ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, ioProperty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Foreign.Ptr" $ do
    prop "Ptr Int instance"     (prop_matchesShow :: Int -> Ptr Int -> Bool)
    prop "FunPtr Int instance"  (prop_matchesShow :: Int -> FunPtr Int -> Bool)
    prop "IntPtr instance"      (prop_matchesShow :: Int -> IntPtr -> Bool)
    prop "WordPtr instance"     (prop_matchesShow :: Int -> WordPtr -> Bool)
    prop "ForeignPtr instance"  prop_showForeignPtr

-- | Verifies the 'Show' instance for 'ForeignPtr' is accurate.
prop_showForeignPtr :: Int -> Ptr Int -> Property
prop_showForeignPtr p ptr = ioProperty $ do
    fptr <- newForeignPtr_ ptr
    pure $ prop_matchesShow p fptr
