{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Spec.GHC.GenericsSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "GHC.Generics" module.
-}
module Spec.GHC.GenericsSpec (main, spec) where

import Data.Orphans ()
import Data.Proxy (Proxy(..))

import GHC.Generics ( U1, Par1, Rec1, K1, M1, (:+:), (:*:), (:.:)
                    , UChar, UDouble, UFloat, UInt, UWord
                    , Fixity, Associativity
                    , Meta(MetaData), SourceUnpackedness
                    , SourceStrictness, DecidedStrictness
                    )

import Instances.GHC.Generics ()

import Spec.Utils (matchesTextShowSpec, genericTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

type MD = 'MetaData "Example" "Module" "package" 'False

m1Description :: String
m1Description = "M1 () ('MetaData \"Example\" \"Module\" \"package\" 'False) Maybe Int"

spec :: Spec
spec = parallel $ do
    describe "Fixity" $ do
        let p :: Proxy Fixity
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "Associativity" $ do
        let p :: Proxy Associativity
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "SourceUnpackedness" $ do
        let p :: Proxy SourceUnpackedness
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "SourceStrictness" $ do
        let p :: Proxy SourceStrictness
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "DecidedStrictness" $ do
        let p :: Proxy DecidedStrictness
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "U1 Int" $ do
        let p :: Proxy (U1 Int)
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "Par1 Int" $ do
        let p :: Proxy (Par1 Int)
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "Rec1 Maybe Int" $ do
        let p :: Proxy (Rec1 Maybe Int)
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "K1 () Int ()" $ do
        let p :: Proxy (K1 () Int ())
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe m1Description $ do
        let p :: Proxy (M1 () MD Maybe Int)
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "(Maybe :+: Maybe) Int" $ do
        let p :: Proxy ((Maybe :+: Maybe) Int)
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "(Maybe :*: Maybe) Int" $ do
        let p :: Proxy ((Maybe :*: Maybe) Int)
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "(Maybe :.: Maybe) Int" $ do
        let p :: Proxy ((Maybe :.: Maybe) Int)
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "UChar Int" $ do
        let p :: Proxy (UChar Int)
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "UDouble Int" $ do
        let p :: Proxy (UDouble Int)
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "UFloat Int" $ do
        let p :: Proxy (UFloat Int)
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "UInt Int" $ do
        let p :: Proxy (UInt Int)
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
    describe "UWord Int" $ do
        let p :: Proxy (UWord Int)
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
