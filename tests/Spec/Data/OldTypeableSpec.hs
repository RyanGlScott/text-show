{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#endif

{-|
Module:      Spec.Data.OldTypeableSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.Monoid" module.
-}
module Spec.Data.OldTypeableSpec (main, spec) where

import Instances.Data.OldTypeable ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
import Data.OldTypeable (TyCon, TypeRep)

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
    describe "TypeRep" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TypeRep -> Bool)
    describe "TyCon" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> TyCon -> Bool)
#else
    pure ()
#endif
