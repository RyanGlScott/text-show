{-|
Module:      Spec.Derived.RankNTypesSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types with rank-n voodoo.
-}
module Spec.Derived.RankNTypesSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Derived.RankNTypes
import Spec.Utils (matchesTextShow1Spec)
import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyCon Int Int" $
        matchesTextShow1Spec (Proxy :: Proxy (TyCon Int Int))
    describe "TyFamily Int Int" $
        matchesTextShow1Spec (Proxy :: Proxy (TyFamily Int Int))
