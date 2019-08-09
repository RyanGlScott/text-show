{-|
Module:      Spec.Data.ComplexSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Complex'.
-}
module Spec.Data.ComplexSpec (main, spec) where

import Data.Complex (Complex)
import Data.Proxy.Compat (Proxy(..))

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Complex Double" $
    matchesTextShowSpec (Proxy :: Proxy (Complex Double))
