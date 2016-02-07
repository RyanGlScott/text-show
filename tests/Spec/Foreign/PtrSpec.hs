{-|
Module:      Spec.Foreign.PtrSpec
Copyright:   (C) 2014-2016 Ryan Scott
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

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, ioProperty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Ptr Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Ptr Int -> Bool)
    describe "FunPtr Int" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> FunPtr Int -> Bool)
    describe "IntPtr" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> IntPtr -> Bool)
    describe "WordPtr" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> WordPtr -> Bool)
    describe "ForeignPtr" $
        prop "TextShow instance" prop_showForeignPtr

-- | Verifies the 'Show' instance for 'ForeignPtr' is accurate.
prop_showForeignPtr :: Int -> Ptr Int -> Property
prop_showForeignPtr p ptr = ioProperty $ do
    fptr <- newForeignPtr_ ptr
    pure $ prop_matchesTextShow p fptr
