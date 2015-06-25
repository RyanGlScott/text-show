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

import Spec.Utils (prop_matchesShow {-, prop_genericShow -})

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Proxy" $ do
    prop "Proxy Int instance"     (prop_matchesShow :: Int -> Proxy Int -> Bool)
    -- TODO: Uncomment this once tagged is fixed
--     prop "Proxy Int generic show" (prop_genericShow :: Int -> Proxy Int -> Bool)
