{-|
Module:      Spec.Data.List.NonEmptySpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'NonEmpty'.
-}
module Spec.Data.List.NonEmptySpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Data.List.NonEmpty.Compat (NonEmpty)
import Data.Orphans ()
import Spec.Utils (matchesTextShow1Spec)
import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "NonEmpty Int" $
    matchesTextShow1Spec (Proxy :: Proxy (NonEmpty Int))
