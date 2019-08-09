{-|
Module:      Spec.Data.TupleSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for tuple types.
-}
module Spec.Data.TupleSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Generics.Deriving.Instances ()
import Instances.Data.Tuple ()
import Spec.Utils (matchesTextShowSpec, matchesTextShow1Spec,
                   genericTextShowSpec, genericTextShow1Spec)
import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "()" $ do
        let p :: Proxy ()
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
    describe "(Int, Int)" $ do
        let p :: Proxy (Int, Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "(Int, Int, Int)" $ do
        let p :: Proxy (Int, Int, Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "(Int, Int, Int, Int)" $ do
        let p :: Proxy (Int, Int, Int, Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "(Int, Int, Int, Int, Int)" $ do
        let p :: Proxy (Int, Int, Int, Int, Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "(Int, Int, Int, Int, Int, Int)" $ do
        let p :: Proxy (Int, Int, Int, Int, Int, Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "(Int, Int, Int, Int, Int, Int, Int)" $ do
        let p :: Proxy (Int, Int, Int, Int, Int, Int, Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "(Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        matchesTextShowSpec (Proxy :: Proxy (Int, Int, Int, Int, Int, Int, Int, Int))
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        matchesTextShowSpec (Proxy :: Proxy (Int, Int, Int, Int, Int, Int, Int, Int, Int))
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        matchesTextShowSpec (Proxy :: Proxy (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int))
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        matchesTextShowSpec (Proxy :: Proxy (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int))
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        matchesTextShowSpec (Proxy :: Proxy (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int))
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        matchesTextShowSpec (Proxy :: Proxy (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int))
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        matchesTextShowSpec (Proxy :: Proxy (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int))
    describe "(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)" $ do
        matchesTextShowSpec (Proxy :: Proxy (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int))
