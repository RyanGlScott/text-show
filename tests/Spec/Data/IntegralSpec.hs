{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.IntegralSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for integral data types.
-}
module Spec.Data.IntegralSpec (main, spec) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

import Prelude ()
import Prelude.Compat

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

#if !defined(mingw32_HOST_OS) && MIN_VERSION_text(1,0,0)
import Control.Applicative (liftA2)
import Data.Char (intToDigit)
import Numeric (showIntAtBase)
import Test.QuickCheck (Gen, arbitrary, getNonNegative, suchThat)

import Text.Show.Text (fromString)
import Text.Show.Text.Data.Integral (showbIntAtBase)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Integral" $ do
    prop "Int instance"          (prop_matchesShow :: Int -> Int -> Bool)
    prop "Int8 instance"         (prop_matchesShow :: Int -> Int8 -> Bool)
    prop "Int16 instance"        (prop_matchesShow :: Int -> Int16 -> Bool)
    prop "Int32 instance"        (prop_matchesShow :: Int -> Int32 -> Bool)
    prop "Int64 instance"        (prop_matchesShow :: Int -> Int64 -> Bool)
    prop "Integer instance"      (prop_matchesShow :: Int -> Integer -> Bool)
    prop "Word instance"         (prop_matchesShow :: Int -> Word -> Bool)
    prop "Word8 instance"        (prop_matchesShow :: Int -> Word8 -> Bool)
    prop "Word16 instance"       (prop_matchesShow :: Int -> Word16 -> Bool)
    prop "Word32 instance"       (prop_matchesShow :: Int -> Word32 -> Bool)
    prop "Word64 instance"       (prop_matchesShow :: Int -> Word64 -> Bool)
#if !defined(mingw32_HOST_OS) && MIN_VERSION_text(1,0,0)
-- TODO: Figure out why this diverges on Windows
    prop "showbIntAtBase output" prop_showIntAtBase
#endif

-- | Verifies 'showIntAtBase' and 'showbIntAtBase' generate the same output.
#if !defined(mingw32_HOST_OS) && MIN_VERSION_text(1,0,0)
prop_showIntAtBase :: Gen Bool
prop_showIntAtBase = do
    base <- arbitrary `suchThat` liftA2 (&&) (> 1) (<= 16)
    i    <- getNonNegative <$> arbitrary :: Gen Int
    pure $ fromString (showIntAtBase base intToDigit i "") == showbIntAtBase base intToDigit i
#endif
