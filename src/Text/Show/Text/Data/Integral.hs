{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Integral
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for integral types.
----------------------------------------------------------------------------
module Text.Show.Text.Data.Integral (
      showbIntPrec
    , showbInt8Prec
    , showbInt16Prec
    , showbInt32Prec
    , showbInt64Prec
    , showbIntegerPrec
    , showbIntegralPrec
    , showbIntAtBase
    , showbBin
    , showbHex
    , showbOct
    , showbRatioPrec
    , showbWord
    , showbWord8
    , showbWord16
    , showbWord32
    , showbWord64
    ) where

import           Data.Char (intToDigit)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Monoid ((<>), mempty)
import           Data.Ratio (Ratio, numerator, denominator)
import           Data.Text.Buildable (build)
import           Data.Text.Lazy.Builder (Builder)
import           Data.Word (Word, Word8, Word16, Word32, Word64)

import           GHC.Exts (Int(I#))
#if __GLASGOW_HASKELL__ >= 708
import           GHC.Exts (isTrue#)
import           GHC.Prim (Int#)
#endif
import           GHC.Prim ((<#), (>#))
import           GHC.Real (ratioPrec, ratioPrec1)

import qualified Prelude as P (show)
import           Prelude hiding (Show)

import           Text.Show.Text.Class (Show(..), showbParen)
import           Text.Show.Text.Functions (s)

-- | Convert an 'Int' to a 'Builder' with the given precedence.
showbIntPrec :: Int -> Int -> Builder
showbIntPrec (I# p) n'@(I# n)
    | isTrue (n <# 0#) && isTrue (p ># 6#) = s '(' <> build n' <> s ')'
    | otherwise = build n'
  where
#if __GLASGOW_HASKELL__ >= 708
      isTrue :: Int# -> Bool
      isTrue b = isTrue# b
#else
      isTrue :: Bool -> Bool
      isTrue = id
#endif

-- | Convert an 'Int8' to a 'Builder' with the given precedence.
showbInt8Prec :: Int -> Int8 -> Builder
showbInt8Prec p = showbIntPrec p . fromIntegral
{-# INLINE showbInt8Prec #-}

-- | Convert an 'Int16' to a 'Builder' with the given precedence.
showbInt16Prec :: Int -> Int16 -> Builder
showbInt16Prec p = showbIntPrec p . fromIntegral
{-# INLINE showbInt16Prec #-}

-- | Convert an 'Int32' to a 'Builder' with the given precedence.
showbInt32Prec :: Int -> Int32 -> Builder
showbInt32Prec p = showbIntPrec p . fromIntegral
{-# INLINE showbInt32Prec #-}

-- | Convert an 'Int64' to a 'Builder' with the given precedence.
showbInt64Prec :: Int -> Int64 -> Builder
#if WORD_SIZE_IN_BITS < 64
showbInt64Prec p = showbIntegerPrec p . toInteger
#else
showbInt64Prec p = showbIntPrec p . fromIntegral
#endif
{-# INLINE showbInt64Prec #-}

-- | Convert an 'Integer' to a 'Builder' with the given precedence.
showbIntegerPrec :: Int -> Integer -> Builder
showbIntegerPrec p n
    | p > 6 && n < 0 = s '(' <> build n <> s ')'
    | otherwise      = build n
{-# INLINE showbIntegerPrec #-}

-- | Convert an 'Integral' type to a 'Builder' with the given precedence.
showbIntegralPrec :: Integral a => Int -> a -> Builder
showbIntegralPrec p = showbIntegerPrec p . toInteger
{-# INLINE showbIntegralPrec #-}

-- | Shows a /non-negative/ 'Integral' number using the base specified by the
--   first argument, and the character representation specified by the second.
showbIntAtBase :: (Integral a, Show a) => a -> (Int -> Char) -> a -> Builder
showbIntAtBase base toChr n0
    | base <= 1 = error . P.show $ "Text.Show.Text.Int.showbIntAtBase: applied to unsupported base" <> showb base
    | n0 < 0    = error . P.show $ "Text.Show.Text.Int.showbIntAtBase: applied to negative number " <> showb n0
    | otherwise = showbIt (quotRem n0 base) mempty
  where
    showbIt (n, d) b = seq c $ -- stricter than necessary
        case n of
             0 -> b'
             _ -> showbIt (quotRem n base) b'
      where
        c :: Char
        c = toChr $ fromIntegral d
        
        b' :: Builder
        b' = s c <> b
{-# INLINE showbIntAtBase #-}

-- | Show /non-negative/ 'Integral' numbers in base 2.
showbBin :: (Integral a, Show a) => a -> Builder
showbBin = showbIntAtBase 2 intToDigit
{-# INLINE showbBin #-}

-- | Show /non-negative/ 'Integral' numbers in base 16.
showbHex :: (Integral a, Show a) => a -> Builder
showbHex = showbIntAtBase 16 intToDigit
{-# INLINE showbHex #-}

-- | Show /non-negative/ 'Integral' numbers in base 8.
showbOct :: (Integral a, Show a) => a -> Builder
showbOct = showbIntAtBase 8 intToDigit
{-# INLINE showbOct #-}

-- | Convert a 'Ratio' to a 'Builder' with the given precedence.
showbRatioPrec :: (Show a, Integral a) => Int -> Ratio a -> Builder
showbRatioPrec p q = showbParen (p > ratioPrec) $
       showbPrec ratioPrec1 (numerator q)
    <> " % "
    <> showbPrec ratioPrec1 (denominator q)
{-# INLINE showbRatioPrec #-}

-- | Convert a 'Word' to a 'Builder' with the given precedence.
showbWord :: Word -> Builder
showbWord = build
{-# INLINE showbWord #-}

-- | Convert a 'Word8' to a 'Builder' with the given precedence.
showbWord8 :: Word8 -> Builder
showbWord8 = build
{-# INLINE showbWord8 #-}

-- | Convert a 'Word16' to a 'Builder' with the given precedence.
showbWord16 :: Word16 -> Builder
showbWord16 = build
{-# INLINE showbWord16 #-}

-- | Convert a 'Word32' to a 'Builder' with the given precedence.
showbWord32 :: Word32 -> Builder
showbWord32 = build
{-# INLINE showbWord32 #-}

-- | Convert a 'Word64' to a 'Builder' with the given precedence.
showbWord64 :: Word64 -> Builder
showbWord64 = build
{-# INLINE showbWord64 #-}

instance Show Int where
    showbPrec = showbIntPrec
    {-# INLINE showbPrec #-}

instance Show Int8 where
    showbPrec = showbInt8Prec
    {-# INLINE showbPrec #-}

instance Show Int16 where
    showbPrec = showbInt16Prec
    {-# INLINE showbPrec #-}

instance Show Int32 where
    showbPrec = showbInt32Prec
    {-# INLINE showbPrec #-}

instance Show Int64 where
    showbPrec = showbInt64Prec
    {-# INLINE showbPrec #-}

instance Show Integer where
    showbPrec = showbIntegerPrec
    {-# INLINE showbPrec #-}

instance (Show a, Integral a) => Show (Ratio a) where
    {-# SPECIALIZE instance Show Rational #-}
    showbPrec = showbRatioPrec
    {-# INLINE showbPrec #-}

instance Show Word where
    showb = showbWord
    {-# INLINE showb #-}

instance Show Word8 where
    showb = showbWord8
    {-# INLINE showb #-}

instance Show Word16 where
    showb = showbWord16
    {-# INLINE showb #-}

instance Show Word32 where
    showb = showbWord32
    {-# INLINE showb #-}

instance Show Word64 where
    showb = showbWord64
    {-# INLINE showb #-}