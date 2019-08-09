{-# LANGUAGE CPP       #-}

#if MIN_VERSION_base(4,6,0) && !(MIN_VERSION_base(4,7,0))
{-# LANGUAGE DataKinds #-}
#endif

{-|
Module:      Spec.GHC.TypeLitsSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "GHC.TypeLits" module.
-}
module Spec.GHC.TypeLitsSpec (main, spec) where

import Instances.GHC.TypeLits ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,6,0)
import Data.Proxy.Compat (Proxy(..))
import GHC.TypeLits
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (describe)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_base(4,7,0)
    describe "SomeNat" $
        matchesTextShowSpec (Proxy :: Proxy SomeNat)
    describe "SomeSymbol" $
        matchesTextShowSpec (Proxy :: Proxy SomeSymbol)
#elif MIN_VERSION_base(4,6,0)
    describe "IsEven 0" $
        matchesTextShowSpec (Proxy :: Proxy (IsEven 0))
    describe "IsEven 1" $
        matchesTextShowSpec (Proxy :: Proxy (IsEven 1))
    describe "IsEven 2" $
        matchesTextShowSpec (Proxy :: Proxy (IsEven 2))
    describe "IsZero 0" $
        matchesTextShowSpec (Proxy :: Proxy (IsZero 0))
    describe "IsZero 1" $
        matchesTextShowSpec (Proxy :: Proxy (IsZero 1))
    describe "Sing 0" $
        matchesTextShowSpec (Proxy :: Proxy (Sing 0))
    describe "Sing \"a\"" $
        matchesTextShowSpec (Proxy :: Proxy (Sing "a"))
#else
    pure ()
#endif
