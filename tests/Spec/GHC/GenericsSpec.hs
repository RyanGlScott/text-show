{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds     #-}
#endif

{-|
Module:      Spec.GHC.GenericsSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "GHC.Generics" module.
-}
module Spec.GHC.GenericsSpec (main, spec) where

import Data.Orphans ()

import Generics.Deriving.Base ( U1, Par1, Rec1, K1, M1, (:+:), (:*:), (:.:)
                              , UChar, UDouble, UFloat, UInt, UWord
                              , Fixity, Associativity
#if MIN_VERSION_base(4,9,0)
                              , Meta(MetaData), SourceUnpackedness
                              , SourceStrictness, DecidedStrictness
#else
                              , Arity
#endif
                              )
import Generics.Deriving.Instances ()

import Instances.GHC.Generics ()

import Spec.Utils (prop_matchesTextShow, prop_genericTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

#if MIN_VERSION_base(4,9,0)
type MD = 'MetaData "Example" "Module" "package" 'False

m1Description :: String
m1Description = "M1 () ('MetaData \"Example\" \"Module\" \"package\" 'False) Maybe Int"
#else
type MD = ()

m1Description :: String
m1Description = "M1 () () Maybe Int"
#endif

spec :: Spec
spec = parallel $ do
    describe "Fixity" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Fixity -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Fixity -> Bool)
    describe "Associativity" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Associativity -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Associativity -> Bool)
#if MIN_VERSION_base(4,9,0)
    describe "SourceUnpackedness" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> SourceUnpackedness -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> SourceUnpackedness -> Bool)
    describe "SourceStrictness" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> SourceStrictness -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> SourceStrictness -> Bool)
    describe "DecidedStrictness" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> DecidedStrictness -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> DecidedStrictness -> Bool)
#else
    describe "Arity" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> Arity -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> Arity -> Bool)
#endif
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
    describe m1Description $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> M1 () MD Maybe Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> M1 () MD Maybe Int -> Bool)
    describe "(Maybe :+: Maybe) Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> (Maybe :+: Maybe) Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> (Maybe :+: Maybe) Int -> Bool)
    describe "(Maybe :*: Maybe) Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> (Maybe :*: Maybe) Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> (Maybe :*: Maybe) Int -> Bool)
    describe "(Maybe :.: Maybe) Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> (Maybe :.: Maybe) Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> (Maybe :.: Maybe) Int -> Bool)
    describe "UChar Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> UChar Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> UChar Int -> Bool)
    describe "UDouble Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> UDouble Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> UDouble Int -> Bool)
    describe "UFloat Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> UFloat Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> UFloat Int -> Bool)
    describe "UInt Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> UInt Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> UInt Int -> Bool)
    describe "UWord Int" $ do
        prop "TextShow instance" (prop_matchesTextShow  :: Int -> UWord Int -> Bool)
        prop "generic TextShow"  (prop_genericTextShow  :: Int -> UWord Int -> Bool)
