{-|
Module:      Spec.Derived.TypeFamiliesSpec
Copyright:   (C) 2020 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests involving corner case-provoking type families.
-}
module Spec.Derived.TypeFamiliesSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))

import Derived.TypeFamilies

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShow1Spec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyConOverSat Int Int" $
        matchesTextShow1Spec (Proxy :: Proxy (TyConOverSat Int Int))
    describe "TyFamilyOverSat Int Int" $
        matchesTextShow1Spec (Proxy :: Proxy (TyFamilyOverSat Int Int))
