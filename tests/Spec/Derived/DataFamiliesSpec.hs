{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
#endif

{-|
Module:      Spec.Derived.DataFamiliesSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for corner case-provoking data families.
-}
module Spec.Derived.DataFamiliesSpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_template_haskell(2,7,0)
import Data.Proxy (Proxy(..))

import Derived.DataFamilies (NotAllShow)

import Spec.Utils (matchesTextShow1Spec, genericTextShowSpec, genericTextShow1Spec)

import Test.Hspec (describe)

# if __GLASGOW_HASKELL__ >= 706
import Derived.DataFamilies (KindDistinguished)
# endif

# if __GLASGOW_HASKELL__ >= 708
import Derived.DataFamilies (NullaryData)
import Spec.Utils (matchesTextShowSpec)
# endif
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_template_haskell(2,7,0)
    describe "NotAllShow Int Int Int Int" $ do
        let p :: Proxy (NotAllShow Int Int Int Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
# if __GLASGOW_HASKELL__ >= 706
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
# endif
# if __GLASGOW_HASKELL__ >= 708
    describe "NullaryData" $ do
        let p :: Proxy NullaryData
            p = Proxy
        matchesTextShowSpec p
        genericTextShowSpec p
# endif
#else
    pure ()
#endif
