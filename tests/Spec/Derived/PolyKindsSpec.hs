{-|
Module:      Spec.Derived.PolyKindsSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types with poly-kinded type variables.
-}
module Spec.Derived.PolyKindsSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Derived.PolyKinds
import Spec.Utils (matchesTextShow1Spec, genericTextShowSpec, genericTextShow1Spec)
import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyConCompose Either Either Either Maybe Maybe Int Int" $ do
        let p :: Proxy (TyConCompose Either Either Either Maybe Maybe Int Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "TyConProxy Int Int" $ do
        let p :: Proxy (TyConProxy Int Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "TyConReallyHighKinds (,,,,) Int Int Int Int Int" $ do
        let p :: Proxy (TyConReallyHighKinds (,,,,) Int Int Int Int Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "TyFamilyCompose Either Either Either Maybe Maybe Int Int" $ do
        let p :: Proxy (TyFamilyCompose Either Either Either Maybe Maybe Int Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "TyFamilyProxy Int Int" $ do
        let p :: Proxy (TyFamilyProxy Int Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "TyFamilyReallyHighKinds (,,,,) Int Int Int Int Int" $ do
        let p :: Proxy (TyFamilyReallyHighKinds (,,,,) Int Int Int Int Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
