{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.FloatingSpec
Copyright:   (C) 2014-2015 Ryan Scott
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

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, suchThat)

import Text.Show.Text (Builder, fromString)
import Text.Show.Text.Data.Floating (showbEFloat, showbFFloat, showbGFloat,
                                     showbFFloatAlt, showbGFloatAlt)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Floating" $ do
    prop "Float instance"          (prop_matchesShow :: Int -> Float -> Bool)
    prop "Double instance"         (prop_matchesShow :: Int -> Double -> Bool)
    prop "showbEFloat output"    $ prop_showXFloat showEFloat showbEFloat
    prop "showbFFloat output"    $ prop_showXFloat showFFloat showbFFloat
    prop "showbGFloat output"    $ prop_showXFloat showGFloat showbGFloat
    prop "showbFFloatAlt output" $ prop_showXFloat showFFloatAlt showbFFloatAlt
    prop "showbGFloatAlt output" $ prop_showXFloat showGFloatAlt showbGFloatAlt
    prop "FPFormat instance"       (prop_matchesShow :: Int -> FPFormat -> Bool)

-- | Verifies @showXFloat@ and @showbXFloat@ generate the same output (where @X@
-- is one of E, F, or G).
prop_showXFloat :: (Maybe Int -> Double -> ShowS) -> (Maybe Int -> Double -> Builder) -> Double -> Gen Bool
prop_showXFloat f1 f2 val = do
    digs <- arbitrary `suchThat` (<= 10)
    pure $ fromString (f1 (Just digs) val "") == f2 (Just digs) val
