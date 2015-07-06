{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

{-|
Module:      Spec.Derived.MagicHashSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types with fields that have unlifted types.
-}
module Spec.Derived.MagicHashSpec (main, spec) where

import Derived.MagicHash

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyCon#" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TyCon# -> Bool)
#if MIN_VERSION_template_haskell(2,7,0)
    describe "TyFamily#" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TyFamily# -> Bool)
#endif
