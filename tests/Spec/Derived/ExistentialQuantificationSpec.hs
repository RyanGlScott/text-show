{-|
Module:      Spec.Derived.ExistentialQuantificationSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for existentially quantified data types.
-}
module Spec.Derived.ExistentialQuantificationSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Derived.ExistentialQuantification
import Spec.Utils (matchesTextShow1Spec)
import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyCon Int Int Int Int" $
        matchesTextShow1Spec (Proxy :: Proxy (TyCon Int Int Int Int))
    describe "TyFamily Int Int Int Int" $
        matchesTextShow1Spec (Proxy :: Proxy (TyFamily Int Int Int Int))
