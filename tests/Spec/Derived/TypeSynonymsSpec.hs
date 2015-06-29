{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Derived.TypeSynonymsSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types that use type synonyms.
-}
module Spec.Derived.TypeSynonymsSpec (main, spec) where

import Derived.TypeSynonyms

import Spec.Utils (prop_matchesShow, prop_genericShow, prop_genericShow1)
#if MIN_VERSION_template_haskell(2,7,0)
import Spec.Utils (prop_genericShow')
#endif

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyCon Int Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> TyCon Int Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> TyCon Int Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> TyCon Int Int -> Bool)
#if MIN_VERSION_template_haskell(2,7,0)
    describe "TyFamily Int Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> TyFamily Int Int -> Bool)
        prop "generic Show"  (prop_genericShow' :: Int -> TyFamily Int Int -> Bool)
        prop "generic Show1" (prop_genericShow1 :: Int -> TyFamily Int Int -> Bool)
#endif
