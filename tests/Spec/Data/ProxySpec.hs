{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.ProxySpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Proxy'.
-}
module Spec.Data.ProxySpec (main, spec) where

import Data.Proxy (Proxy)

import Instances.Data.Proxy ()

import Spec.Utils (prop_matchesTextShow)
#if __GLASGOW_HASKELL__ >= 702
import Spec.Utils (prop_genericTextShow)
#endif

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Proxy Int" $ do
    prop "TextShow instance" (prop_matchesTextShow :: Int -> Proxy Int -> Bool)
#if __GLASGOW_HASKELL__ >= 702
    prop "generic TextShow"  (prop_genericTextShow :: Int -> Proxy Int -> Bool)
#endif
