{-|
Module:      Spec.GHC.FingerprintSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Fingerprint'.
-}
module Spec.GHC.FingerprintSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Data.Orphans ()

import GHC.Fingerprint.Type (Fingerprint)

import Instances.GHC.Fingerprint ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "Fingerprint" $
        matchesTextShowSpec (Proxy :: Proxy Fingerprint)
