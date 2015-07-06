{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.FingerprintSpec
Copyright:   (C) 2014-2015 Ryan Scott
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
import GHC.Fingerprint.Type (Fingerprint)

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if MIN_VERSION_base(4,4,0)
    describe "Fingerprint" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Fingerprint -> Bool)
#else
    pure ()
#endif
