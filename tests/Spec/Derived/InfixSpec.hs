{-|
Module:      Spec.Derived.InfixSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types with infix constructors.
-}
module Spec.Derived.InfixSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Derived.Infix
import Spec.Utils (matchesTextShowSpec, genericTextShowSpec, genericTextShow1Spec)
import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyConPlain Int Int" $ do
        let p :: Proxy (TyConPlain Int Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "TyConGADT Int Int" $ do
        let p :: Proxy (TyConGADT Int Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "TyFamilyPlain Int Int" $ do
        let p :: Proxy (TyFamilyPlain Int Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "TyFamilyGADT Int Int" $ do
        let p :: Proxy (TyFamilyGADT Int Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
