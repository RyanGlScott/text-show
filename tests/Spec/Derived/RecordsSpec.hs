{-|
Module:      Spec.Derived.RecordsSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types with record syntax.
-}
module Spec.Derived.RecordsSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Derived.Records
import Spec.Utils (matchesTextShow1Spec, genericTextShowSpec, genericTextShow1Spec)
import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyCon Int Int" $ do
        let p :: Proxy (TyCon Int Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
    describe "TyFamily Int Int" $ do
        let p :: Proxy (TyFamily Int Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
