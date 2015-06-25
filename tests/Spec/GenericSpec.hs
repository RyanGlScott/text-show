{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GenericSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'ConType'.
-}
module Spec.GenericSpec (main, spec) where

import Instances.Generic ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if __GLASGOW_HASKELL__ >= 702
import Spec.Utils (prop_matchesShow, prop_genericShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text.Generic (ConType)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if __GLASGOW_HASKELL__ >= 702
    describe "Text.Show.Text.Generic" $
        describe "ConType" $ do
            prop "Show instance" (prop_matchesShow :: Int -> ConType -> Bool)
            prop "generic show"  (prop_genericShow :: Int -> ConType -> Bool)
#else
    pure ()
#endif
