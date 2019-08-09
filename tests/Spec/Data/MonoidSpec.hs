{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.MonoidSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.Monoid" module.
-}
module Spec.Data.MonoidSpec (main, spec) where

import Data.Monoid
import Data.Proxy.Compat (Proxy(..))

import Generics.Deriving.Instances ()

import Instances.Data.Monoid ()

import Spec.Utils (matchesTextShowSpec, genericTextShowSpec, genericTextShow1Spec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "All" $ do
        let p :: Proxy All
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
    describe "Any" $ do
        let p :: Proxy Any
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
    describe "Dual Int" $ do
        let p :: Proxy (Dual Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "First Int" $ do
        let p :: Proxy (First Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "Last Int" $ do
        let p :: Proxy (Last Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "Product Int" $ do
        let p :: Proxy (Product Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "Sum Int" $ do
        let p :: Proxy (Sum Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
#if MIN_VERSION_base(4,8,0)
    describe "Alt Maybe Int" $ do
        let p :: Proxy (Alt Maybe Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
#endif
#if MIN_VERSION_base(4,12,0)
    describe "Ap Maybe Int" $ do
        let p :: Proxy (Ap Maybe Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
#endif
