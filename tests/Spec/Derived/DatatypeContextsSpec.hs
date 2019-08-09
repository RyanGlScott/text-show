{-|
Module:      Spec.Derived.DatatypeContextsSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types with DatatypeContexts (eww).
-}
module Spec.Derived.DatatypeContextsSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Derived.DatatypeContexts
import Spec.Utils (matchesTextShow1Spec)
import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyCon Int Int Int" $
        matchesTextShow1Spec (Proxy :: Proxy (TyCon Int Int Int))
    describe "TyFamily Int Int Int" $
        matchesTextShow1Spec (Proxy :: Proxy (TyFamily Int Int Int))
