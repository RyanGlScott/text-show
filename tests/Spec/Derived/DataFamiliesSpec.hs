{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
#endif

{-|
Module:      Spec.Derived.DataFamiliesSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for corner case-provoking data families.
-}
module Spec.Derived.DataFamiliesSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))

import Derived.DataFamilies (NotAllShow)

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShow1Spec, genericTextShowSpec, genericTextShow1Spec)

import Test.Hspec (Spec, describe, hspec, parallel)

#if __GLASGOW_HASKELL__ >= 706
import Derived.DataFamilies (KindDistinguished)
#endif

#if __GLASGOW_HASKELL__ >= 708
import Derived.DataFamilies (NullaryData)
import Spec.Utils (matchesTextShowSpec)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "NotAllShow Int Int Int Int" $ do
        let p :: Proxy (NotAllShow Int Int Int Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
#if __GLASGOW_HASKELL__ >= 706
    describe "KindDistinguished '() Int Int" $ do
        let p :: Proxy (KindDistinguished '() Int Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "KindDistinguished 'True Int Int" $ do
        let p :: Proxy (KindDistinguished 'True Int Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
#endif
#if __GLASGOW_HASKELL__ >= 708
    describe "NullaryData" $ do
        let p :: Proxy NullaryData
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
#endif
