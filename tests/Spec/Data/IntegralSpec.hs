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

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

#if !defined(mingw32_HOST_OS) && MIN_VERSION_text(1,0,0)
import Control.Applicative (liftA2)
import Data.Char (intToDigit)
import Numeric (showIntAtBase)
import Test.QuickCheck (Gen, arbitrary, getNonNegative, suchThat)

import TextShow (fromString)
import TextShow.Data.Integral (showbIntAtBase)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Int" $
        prop "TextShow instance"                        (prop_matchesTextShow :: Int -> Int -> Bool)
    describe "Int8" $
        prop "TextShow instance"                        (prop_matchesTextShow :: Int -> Int8 -> Bool)
    describe "Int16" $
        prop "TextShow instance"                        (prop_matchesTextShow :: Int -> Int16 -> Bool)
    describe "Int32" $
        prop "TextShow instance"                        (prop_matchesTextShow :: Int -> Int32 -> Bool)
    describe "Int64" $
        prop "TextShow instance"                        (prop_matchesTextShow :: Int -> Int64 -> Bool)
    describe "Integer" $
        prop "TextShow instance"                        (prop_matchesTextShow :: Int -> Integer -> Bool)
    describe "Word" $
        prop "TextShow instance"                        (prop_matchesTextShow :: Int -> Word -> Bool)
    describe "Word8" $
        prop "TextShow instance"                        (prop_matchesTextShow :: Int -> Word8 -> Bool)
    describe "Word16" $
        prop "TextShow instance"                        (prop_matchesTextShow :: Int -> Word16 -> Bool)
    describe "Word32" $
        prop "TextShow instance"                        (prop_matchesTextShow :: Int -> Word32 -> Bool)
    describe "Word64" $
        prop "TextShow instance"                        (prop_matchesTextShow :: Int -> Word64 -> Bool)
#if !defined(mingw32_HOST_OS) && MIN_VERSION_text(1,0,0)
-- TODO: Figure out why this diverges on Windows
    describe "showbIntAtBase" $
        prop "has the same output as showIntAtBase" prop_showIntAtBase
#endif

-- | Verifies 'showIntAtBase' and 'showbIntAtBase' generate the same output.
#if !defined(mingw32_HOST_OS) && MIN_VERSION_text(1,0,0)
prop_showIntAtBase :: Gen Bool
prop_showIntAtBase = do
    base <- arbitrary `suchThat` liftA2 (&&) (> 1) (<= 16)
    i    <- getNonNegative <$> arbitrary :: Gen Int
    pure $ fromString (showIntAtBase base intToDigit i "") == showbIntAtBase base intToDigit i
#endif
