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

import Spec.Utils (prop_matchesTextShow, prop_genericTextShow, prop_genericTextShow1)
#if MIN_VERSION_template_haskell(2,7,0)
import Spec.Utils (prop_genericTextShow')
#endif

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyCon Int Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> TyCon Int Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> TyCon Int Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> TyCon Int Int -> Bool)
#if MIN_VERSION_template_haskell(2,7,0)
    describe "TyFamily Int Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> TyFamily Int Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow' :: Int -> TyFamily Int Int -> Bool)
        prop "generic TextShow1" (prop_genericTextShow1 :: Int -> TyFamily Int Int -> Bool)
#endif
