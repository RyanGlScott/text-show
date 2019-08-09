{-|
Module:      Spec.Numeric.NaturalSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Natural'.
-}
module Spec.Numeric.NaturalSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Numeric.Natural.Compat (Natural)
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Natural" $
    matchesTextShowSpec (Proxy :: Proxy Natural)
