{-|
Module:      Spec.Data.FixedSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'Fixed' values.
-}
module Spec.Data.FixedSpec (main, spec) where

import Data.Fixed (Fixed, E0, E1, E2, E3, E6, E9, E12, showFixed)
import Data.Proxy.Compat (Proxy(..))

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow (fromString)
import TextShow.Data.Fixed (showbFixed)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Fixed E0" $
        matchesTextShowSpec (Proxy :: Proxy (Fixed E0))
    describe "Fixed E1" $
        matchesTextShowSpec (Proxy :: Proxy (Fixed E1))
    describe "Fixed E2" $
        matchesTextShowSpec (Proxy :: Proxy (Fixed E2))
    describe "Fixed E3" $
        matchesTextShowSpec (Proxy :: Proxy (Fixed E3))
    describe "Fixed E6" $
        matchesTextShowSpec (Proxy :: Proxy (Fixed E6))
    describe "Fixed E9" $
        matchesTextShowSpec (Proxy :: Proxy (Fixed E9))
    describe "Fixed E12" $
        matchesTextShowSpec (Proxy :: Proxy (Fixed E12))
    describe "showbFixed" $
        prop "has the same output as showFixed" prop_showFixed

-- | Verifies 'showFixed' and 'showbFixed' generate the same output.
prop_showFixed :: Bool -> Fixed E12 -> Bool
prop_showFixed b f = fromString (showFixed b f) == showbFixed b f
