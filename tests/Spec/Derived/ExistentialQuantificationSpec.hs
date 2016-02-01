{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Derived.ExistentialQuantificationSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for existentially quantified data types.
-}
module Spec.Derived.ExistentialQuantificationSpec (main, spec) where

import Derived.ExistentialQuantification

import Spec.Utils (prop_matchesTextShow1)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyCon Int Int Int Int" $
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> TyCon Int Int Int Int -> Bool)
#if MIN_VERSION_template_haskell(2,7,0)
    describe "TyFamily Int Int Int Int" $
        prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> TyFamily Int Int Int Int -> Bool)
#endif
