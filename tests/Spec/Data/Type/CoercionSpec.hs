{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.Type.CoercionSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Coercion'.
-}
module Spec.Data.Type.CoercionSpec (main, spec) where

import Instances.Data.Type.Coercion ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,7,0)
import Data.Monoid (All(..))
import Data.Type.Coercion (Coercion)

import Spec.Utils (prop_matchesShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if MIN_VERSION_base(4,7,0)
    describe "Text.Show.Text.Data.Type.Coercion" $
        prop "Coercion instance" (prop_matchesShow :: Int -> Coercion All Bool -> Bool)
#else
    pure ()
#endif
