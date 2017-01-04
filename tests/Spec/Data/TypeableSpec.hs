{-# LANGUAGE CPP #-}

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

import Data.Proxy (Proxy(..))
import Data.Typeable (TyCon, TypeRep)

#if MIN_VERSION_base(4,9,0)
import GHC.Types (TrName, Module)
#endif

import Instances.Data.Typeable ()

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TypeRep" $
        matchesTextShowSpec (Proxy :: Proxy TypeRep)
    describe "TyCon" $
        matchesTextShowSpec (Proxy :: Proxy TyCon)
#if MIN_VERSION_base(4,9,0)
    describe "TrName" $
        matchesTextShowSpec (Proxy :: Proxy TrName)
    describe "Module" $
        matchesTextShowSpec (Proxy :: Proxy Module)
#endif
