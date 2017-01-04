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

import Data.Proxy (Proxy(..))
import Data.List.NonEmpty (NonEmpty)
import Instances.Data.List.NonEmpty ()
import Spec.Utils (matchesTextShow1Spec)
import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "NonEmpty Int" $
    matchesTextShow1Spec (Proxy :: Proxy (NonEmpty Int))
