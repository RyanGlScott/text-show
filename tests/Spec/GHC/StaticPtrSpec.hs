{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.StaticPtrSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'StaticPtr'.
-}
module Spec.GHC.StaticPtrSpec (main, spec) where

import Instances.GHC.StaticPtr ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,8,0)
import GHC.StaticPtr (StaticPtrInfo)

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if MIN_VERSION_base(4,8,0)
    describe "StaticPtrInfo" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> StaticPtrInfo -> Bool)
#else
    pure ()
#endif
