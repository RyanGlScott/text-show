{-|
Module:      Spec.Data.CharSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.Char" module.
-}
module Spec.Data.CharSpec (main, spec) where

import Data.Array (elems)
import Data.Char (GeneralCategory)
import Data.Proxy.Compat (Proxy(..))

import GHC.Show (asciiTab)

import Instances.Data.Char ()

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, it, parallel, shouldBe)

import TextShow (fromString)
import TextShow.Data.Char (asciiTabB)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Char" $
        matchesTextShowSpec (Proxy :: Proxy Char)
    describe "GeneralCategory" $
        matchesTextShowSpec (Proxy :: Proxy GeneralCategory)
    describe "asciiTabB" $
        it "equals asciiTab" $ map fromString asciiTab `shouldBe` elems asciiTabB
