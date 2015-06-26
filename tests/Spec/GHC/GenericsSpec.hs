{-# LANGUAGE CPP           #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE TypeOperators #-}
#endif

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

import Instances.GHC.Generics ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (U1, Par1, Rec1, K1, M1, (:+:), (:*:), (:.:),
                     Fixity, Associativity, Arity)

import Spec.Utils (prop_matchesShow, prop_genericShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if __GLASGOW_HASKELL__ >= 702
    describe "Fixity" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Fixity -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Fixity -> Bool)
    describe "Associativity" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Associativity -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Associativity -> Bool)
    describe "Arity" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Arity -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Arity -> Bool)
    describe "U1 Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> U1 Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> U1 Int -> Bool)
    describe "Par1 Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Par1 Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Par1 Int -> Bool)
    describe "Rec1 Maybe Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> Rec1 Maybe Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> Rec1 Maybe Int -> Bool)
    describe "K1 () Int ()" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> K1 () Int () -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> K1 () Int () -> Bool)
    describe "M1 () () Maybe Int" $ do
        prop "Show instance" (prop_matchesShow  :: Int -> M1 () () Maybe Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> M1 () () Maybe Int -> Bool)
    describe "(Maybe :+: Maybe) Int " $ do
        prop "Show instance" (prop_matchesShow  :: Int -> (Maybe :+: Maybe) Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> (Maybe :+: Maybe) Int -> Bool)
    describe "(Maybe :*: Maybe) Int " $ do
        prop "Show instance" (prop_matchesShow  :: Int -> (Maybe :*: Maybe) Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> (Maybe :*: Maybe) Int -> Bool)
    describe "(Maybe :.: Maybe) Int " $ do
        prop "Show instance" (prop_matchesShow  :: Int -> (Maybe :.: Maybe) Int -> Bool)
        prop "generic Show"  (prop_genericShow  :: Int -> (Maybe :.: Maybe) Int -> Bool)
#else
    pure ()
#endif
