{-|
Module:      Spec.Data.ListSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for lists.
-}
module Spec.Data.ListSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show (showListWith)
import TextShow (fromString, showb)
import TextShow.Data.List (showbListWith)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "String" $
        matchesTextShowSpec (Proxy :: Proxy String)
    describe "[String]" $
        matchesTextShowSpec (Proxy :: Proxy [String])
    describe "[Int]" $
        matchesTextShowSpec (Proxy :: Proxy [Int])
    describe "showbListWith" $
        prop "has the same output as showListWith" prop_showListWith

-- | Verifies 'showListWith' and 'showbListWith' generate the same output.
prop_showListWith :: String -> Bool
prop_showListWith str = fromString (showListWith shows str "") == showbListWith showb str
