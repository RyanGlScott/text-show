{-|
Module:      Spec.System.ExitSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'ExitCode'.
-}
module Spec.System.ExitSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Spec.Utils (matchesTextShowSpec)
import System.Exit (ExitCode)
import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "ExitCode" $
    matchesTextShowSpec (Proxy :: Proxy ExitCode)
