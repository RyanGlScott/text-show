{-# LANGUAGE CPP #-}

{-|
Module:      Spec.FromStringTextShowSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'FromStringShow' and 'FromTextShow'.
-}
module Spec.FromStringTextShowSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Instances.FromStringTextShow ()
import Spec.Utils (matchesTextShowSpec, matchesTextShow1Spec)
import Test.Hspec (Spec, describe, hspec, parallel)
import TextShow (FromStringShow(..), FromTextShow(..))

#if defined(NEW_FUNCTOR_CLASSES)
import Spec.Utils (matchesTextShow2Spec)
import TextShow (FromStringShow1(..), FromStringShow2(..),
                 FromTextShow1(..), FromTextShow2(..))
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "FromStringShow Int" $ do
        let p :: Proxy (FromStringShow Int)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
    describe "FromStringShow String" $ do
        let p :: Proxy (FromStringShow String)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
    describe "FromTextShow Int" $ do
        let p :: Proxy (FromTextShow Int)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
    describe "FromTextShow String" $ do
        let p :: Proxy (FromTextShow String)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
#if defined(NEW_FUNCTOR_CLASSES)
    describe "FromStringShow1 Maybe Int" $ do
        let p :: Proxy (FromStringShow1 Maybe Int)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
    describe "FromTextShow1 Maybe Int" $ do
        let p :: Proxy (FromTextShow1 Maybe Int)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
    describe "FromStringShow2 Either Char Int" $ do
        let p :: Proxy (FromStringShow2 Either Char Int)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
        matchesTextShow2Spec p
    describe "FromTextShow2 Either Char Int" $ do
        let p :: Proxy (FromTextShow2 Either Char Int)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
        matchesTextShow2Spec p
#endif
