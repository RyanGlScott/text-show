{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.FloatingSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for floating-point data types.
-}
module Spec.Data.FloatingSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Data.Text.Lazy.Builder.RealFloat (FPFormat)

import Instances.Data.Floating ()

import Numeric.Compat (showEFloat, showFFloat, showGFloat,
                       showFFloatAlt, showGFloatAlt)

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, arbitrary, property, suchThat)

import TextShow (Builder, fromString)
import TextShow.Data.Floating (showbEFloat, showbFFloat, showbGFloat,
                               showbFFloatAlt, showbGFloatAlt)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Float" $
        matchesTextShowSpec (Proxy :: Proxy Float)
    describe "Double" $
        matchesTextShowSpec (Proxy :: Proxy Double)
    describe "showbEFloat" $
        prop "has the same output as showEFloat" $    prop_showXFloat showEFloat showbEFloat
    describe "showbFFloat" $
        prop "has the same output as showFFloat" $    prop_showXFloat showFFloat showbFFloat
    describe "showbGFloat" $
        prop "has the same output as showGFloat" $    prop_showXFloat showGFloat showbGFloat
    describe "showbFFloatAlt" $
        prop "has the same output as showFFloatAlt" $ prop_showXFloat showFFloatAlt showbFFloatAlt
    describe "showbGFloatAlt" $
        prop "has the same output as showFFloatAlt" $ prop_showXFloat showGFloatAlt showbGFloatAlt
    describe "FPFormat" $
        matchesTextShowSpec (Proxy :: Proxy FPFormat)

-- | Verifies @showXFloat@ and @showbXFloat@ generate the same output (where @X@
-- is one of E, F, or G).
prop_showXFloat :: (Maybe Int -> Double -> ShowS)
                -> (Maybe Int -> Double -> Builder)
                -> Double -> Property
prop_showXFloat f1 f2 val = property $ do
  mb_digs <- arbitrary `suchThat` cond
  pure $ fromString (f1 mb_digs val "") == f2 mb_digs val
  where
    cond :: Maybe Int -> Bool
    cond mb_digs =
      mb_digs /= Nothing && mb_digs <= Just 10
#if !(MIN_VERSION_base(4,12,0))
      && mb_digs > Just 0 -- Work around Trac #15115
#endif
