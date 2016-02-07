{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.FloatingSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for floating-point data types.
-}
module Spec.Data.FloatingSpec (main, spec) where

import Data.Text.Lazy.Builder.RealFloat (FPFormat)

import Instances.Data.Floating ()

import Numeric.Compat (showEFloat, showFFloat, showGFloat,
                       showFFloatAlt, showGFloatAlt)

import Prelude ()
import Prelude.Compat

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, suchThat)

import TextShow (Builder, fromString)
import TextShow.Data.Floating (showbEFloat, showbFFloat, showbGFloat,
                                     showbFFloatAlt, showbGFloatAlt)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Float" $
        prop "TextShow instance"                          (prop_matchesTextShow :: Int -> Float -> Bool)
    describe "Double" $
        prop "TextShow instance"                          (prop_matchesTextShow :: Int -> Double -> Bool)
    describe "showbEFloat" $
        prop "has the same output as showEFloat" $        prop_showXFloat showEFloat showbEFloat
    describe "showbFFloat" $
        prop "has the same output as showFFloat" $        prop_showXFloat showFFloat showbFFloat
    describe "showbGFloat" $
        prop "has the same output as showGFloat" $        prop_showXFloat showGFloat showbGFloat
    describe "showbFFloatAlt" $
        prop "has the same output as showFFloatAlt" $     prop_showXFloat showFFloatAlt showbFFloatAlt
    describe "showbGFloatAlt" $
        prop "has the same output as showFFloatAlt" $     prop_showXFloat showGFloatAlt showbGFloatAlt
    describe "FPFormat" $
        prop "TextShow instance"                          (prop_matchesTextShow :: Int -> FPFormat -> Bool)

-- | Verifies @showXFloat@ and @showbXFloat@ generate the same output (where @X@
-- is one of E, F, or G).
prop_showXFloat :: (Maybe Int -> Double -> ShowS) -> (Maybe Int -> Double -> Builder) -> Double -> Gen Bool
prop_showXFloat f1 f2 val = do
    digs <- arbitrary `suchThat` (<= 10)
    pure $ fromString (f1 (Just digs) val "") == f2 (Just digs) val
