{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Derived.DataFamiliesSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for corner case-provoking data families.
-}
module Spec.Derived.DataFamiliesSpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_template_haskell(2,7,0)
import Derived.DataFamilies (NotAllShow)

import Spec.Utils (prop_matchesShow2, prop_genericShow', prop_genericShow1)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)

# if __GLASGOW_HASKELL__ >= 708
import Derived.DataFamilies (NullaryData)
import Spec.Utils (prop_matchesShow)
# endif
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_template_haskell(2,7,0)
    describe "NotAllShow Int Int Int Int" $ do
        prop "Show2 instance" (prop_matchesShow2 :: Int -> NotAllShow Int Int Int Int -> Bool)
        prop "generic Show"   (prop_genericShow' :: Int -> NotAllShow Int Int Int Int -> Bool)
        prop "generic Show1"  (prop_genericShow1 :: Int -> NotAllShow Int Int Int Int -> Bool)
# if __GLASGOW_HASKELL__ >= 708
    describe "NullaryData" $ do
        prop "Show instance"  (prop_matchesShow  :: Int -> NullaryData -> Bool)
        prop "generic Show"   (prop_genericShow' :: Int -> NullaryData -> Bool)
# endif
#else
    pure ()
#endif
