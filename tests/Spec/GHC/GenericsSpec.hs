{-# LANGUAGE TypeOperators #-}

{-|
Module:      Spec.GHC.GenericsSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "GHC.Generics" module.
-}
module Spec.GHC.GenericsSpec (main, spec) where

import Data.Orphans ()

import Generics.Deriving.Base (U1, Par1, Rec1, K1, M1, (:+:), (:*:), (:.:),
                               Fixity, Associativity, Arity)
import Generics.Deriving.Instances ()

import Instances.GHC.Generics ()

import Spec.Utils (prop_matchesTextShow, prop_genericTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Fixity" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Fixity -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Fixity -> Bool)
    describe "Associativity" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Associativity -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Associativity -> Bool)
    describe "Arity" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Arity -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Arity -> Bool)
    describe "U1 Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> U1 Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> U1 Int -> Bool)
    describe "Par1 Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Par1 Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Par1 Int -> Bool)
    describe "Rec1 Maybe Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Rec1 Maybe Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Rec1 Maybe Int -> Bool)
    describe "K1 () Int ()" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> K1 () Int () -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> K1 () Int () -> Bool)
    describe "M1 () () Maybe Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> M1 () () Maybe Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> M1 () () Maybe Int -> Bool)
    describe "(Maybe :+: Maybe) Int " $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> (Maybe :+: Maybe) Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> (Maybe :+: Maybe) Int -> Bool)
    describe "(Maybe :*: Maybe) Int " $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> (Maybe :*: Maybe) Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> (Maybe :*: Maybe) Int -> Bool)
    describe "(Maybe :.: Maybe) Int " $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> (Maybe :.: Maybe) Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> (Maybe :.: Maybe) Int -> Bool)
