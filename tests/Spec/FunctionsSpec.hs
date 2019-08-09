{-|
Module:      Spec.FunctionsSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for the orphan 'TextShow' instance for functions.
-}
module Spec.FunctionsSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (Spec, describe, hspec, parallel)
import Text.Show.Functions ()
import TextShow.Functions ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Int -> Int" $
    matchesTextShowSpec (Proxy :: Proxy (Int -> Int))
