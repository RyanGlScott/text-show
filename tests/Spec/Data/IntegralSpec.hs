{-# LANGUAGE CPP #-}

{-|
Module:      Spec.Data.IntegralSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for integral data types.
-}
module Spec.Data.IntegralSpec (main, spec) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word16, Word32, Word64)

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

#if !defined(mingw32_HOST_OS)
import Data.Char (intToDigit)

import Numeric (showIntAtBase)

import Test.QuickCheck (Gen, arbitrary, getNonNegative, suchThat)
import Test.Hspec (Expectation, shouldBe)
import Test.Hspec.QuickCheck (prop)

import TextShow (fromString)
import TextShow.Data.Integral (showbIntAtBase)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Int" $
        matchesTextShowSpec (Proxy :: Proxy Int)
    describe "Int8" $
        matchesTextShowSpec (Proxy :: Proxy Int8)
    describe "Int16" $
        matchesTextShowSpec (Proxy :: Proxy Int16)
    describe "Int32" $
        matchesTextShowSpec (Proxy :: Proxy Int32)
    describe "Int64" $
        matchesTextShowSpec (Proxy :: Proxy Int64)
    describe "Integer" $
        matchesTextShowSpec (Proxy :: Proxy Integer)
    describe "Word" $
        matchesTextShowSpec (Proxy :: Proxy Word)
    describe "Word8" $
        matchesTextShowSpec (Proxy :: Proxy Word8)
    describe "Word16" $
        matchesTextShowSpec (Proxy :: Proxy Word16)
    describe "Word32" $
        matchesTextShowSpec (Proxy :: Proxy Word32)
    describe "Word64" $
        matchesTextShowSpec (Proxy :: Proxy Word64)
#if !defined(mingw32_HOST_OS)
-- TODO: Figure out why this diverges on Windows
    describe "showbIntAtBase" $
        prop "has the same output as showIntAtBase" prop_showIntAtBase
#endif

-- | Verifies 'showIntAtBase' and 'showbIntAtBase' generate the same output.
#if !defined(mingw32_HOST_OS)
prop_showIntAtBase :: Gen Expectation
prop_showIntAtBase = do
    base <- arbitrary `suchThat` \b -> 1 < b && b <= 16
    i    <- getNonNegative <$> arbitrary :: Gen Int
    pure $ fromString (showIntAtBase base intToDigit i "") `shouldBe` showbIntAtBase base intToDigit i
#endif
