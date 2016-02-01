{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Derived.RankNTypesSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types with rank-n voodoo.
-}
module Spec.Derived.RankNTypesSpec (main, spec) where

import Derived.RankNTypes

import Spec.Utils (prop_matchesTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyCon Int Int" $
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> TyCon Int Int -> Bool)
#if MIN_VERSION_template_haskell(2,7,0)
    describe "TyFamily Int Int" $
        prop "TextShow2 instance" (prop_matchesTextShow1 :: Int -> TyFamily Int Int -> Bool)
#endif
