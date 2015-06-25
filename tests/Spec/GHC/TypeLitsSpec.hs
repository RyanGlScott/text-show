{-# LANGUAGE CPP       #-}

#if MIN_VERSION_base(4,6,0) && !(MIN_VERSION_base(4,7,0))
{-# LANGUAGE DataKinds #-}
#endif

{-|
Module:      Spec.GHC.TypeLitsSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "GHC.TypeLits" module.
-}
module Spec.GHC.TypeLitsSpec (main, spec) where

import Instances.GHC.TypeLits ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,6,0)
import GHC.TypeLits

import Spec.Utils (prop_matchesShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if MIN_VERSION_base(4,6,0)
    describe "Text.Show.Text.GHC.TypeLits" $ do
# if MIN_VERSION_base(4,7,0)
        prop "SomeNat instance"    (prop_matchesShow :: Int -> SomeNat -> Bool)
        prop "SomeSymbol instance" (prop_matchesShow :: Int -> SomeSymbol -> Bool)
# else
        prop "IsEven 0 instance"   (prop_matchesShow :: Int -> IsEven 0 -> Bool)
        prop "IsEven 1 instance"   (prop_matchesShow :: Int -> IsEven 1 -> Bool)
        prop "IsEven 2 instance"   (prop_matchesShow :: Int -> IsEven 2 -> Bool)
        prop "IsZero 0 instance"   (prop_matchesShow :: Int -> IsZero 0 -> Bool)
        prop "IsZero 1 instance"   (prop_matchesShow :: Int -> IsZero 1 -> Bool)
        prop "Sing 0 instance"     (prop_matchesShow :: Int -> Sing 0 -> Bool)
        prop "Sing \"a\" instance" (prop_matchesShow :: Int -> Sing "a" -> Bool)
# endif
#else
    pure ()
#endif
