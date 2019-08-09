{-# LANGUAGE CPP       #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
#endif

{-|
Module:      Spec.Data.TypeableSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.Typeable" module.
-}
module Spec.Data.TypeableSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Data.Typeable (TyCon)

#if MIN_VERSION_base(4,9,0)
import GHC.Types (TrName, Module)
#endif

#if MIN_VERSION_base(4,10,0)
import Data.Kind (Type)
import Type.Reflection (SomeTypeRep, TypeRep)
#else
import Data.Typeable (TypeRep)
#endif

import Instances.Data.Typeable ()

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyCon" $
        matchesTextShowSpec (Proxy :: Proxy TyCon)
#if MIN_VERSION_base(4,9,0)
    describe "TrName" $
        matchesTextShowSpec (Proxy :: Proxy TrName)
    describe "Module" $
        matchesTextShowSpec (Proxy :: Proxy Module)
#endif
#if MIN_VERSION_base(4,10,0)
    describe "SomeTypeRep" $
        matchesTextShowSpec (Proxy :: Proxy SomeTypeRep)
    describe "TypeRep" $ do
        describe "TypeRep Type" $
            matchesTextShowSpec (Proxy :: Proxy (TypeRep Type))
        describe "TypeRep [Int]" $
            matchesTextShowSpec (Proxy :: Proxy (TypeRep [Int]))
        describe "TypeRep '[Int]" $
            matchesTextShowSpec (Proxy :: Proxy (TypeRep '[Int]))
        describe "TypeRep (Int, Int)" $
            matchesTextShowSpec (Proxy :: Proxy (TypeRep (Int, Int)))
        describe "TypeRep '(Int, Int)" $
            matchesTextShowSpec (Proxy :: Proxy (TypeRep '(Int, Int)))
        describe "TypeRep (Int -> Int)" $
            matchesTextShowSpec (Proxy :: Proxy (TypeRep (Int -> Int)))
        describe "TypeRep (Either Int)" $
            matchesTextShowSpec (Proxy :: Proxy (TypeRep (Either Int)))
#else
    describe "TypeRep" $
        matchesTextShowSpec (Proxy :: Proxy TypeRep)
#endif
