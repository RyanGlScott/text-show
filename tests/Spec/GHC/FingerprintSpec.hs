{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.FingerprintSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Fingerprint'.
-}
module Spec.GHC.FingerprintSpec (main, spec) where

import Data.Orphans ()

import Instances.GHC.Fingerprint ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,4,0)
import Data.Proxy (Proxy(..))
import GHC.Fingerprint.Type (Fingerprint)
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (describe)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if MIN_VERSION_base(4,4,0)
    describe "Fingerprint" $
        matchesTextShowSpec (Proxy :: Proxy Fingerprint)
#else
    pure ()
#endif
