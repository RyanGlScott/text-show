{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Integral
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for integral types.

/Since: 2/
-}
module TextShow.Data.Integral (
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
    , showbWord
    , showbWord8
    , showbWord16
    , showbWord32
    , showbWord64
    ) where

import           Data.Char (intToDigit)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Monoid.Compat ((<>))
import           Data.Text.Lazy.Builder (Builder, singleton)
import           Data.Text.Lazy.Builder.Int (decimal)
import           Data.Word (Word8, Word16, Word32, Word64)

import           GHC.Exts (Int(I#))
#if __GLASGOW_HASKELL__ >= 708
import           GHC.Exts (isTrue#)
import           GHC.Prim (Int#)
#endif
import           GHC.Prim ((<#), (>#))

import           Prelude ()
import           Prelude.Compat hiding (Show)

import           TextShow.Classes (TextShow(showb, showbPrec))
import           TextShow.Utils (toString)

#include "inline.h"

-- | Convert an 'Int' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbIntPrec :: Int -> Int -> Builder
showbIntPrec (I# p) n'@(I# n)
    | isTrue (n <# 0#) && isTrue (p ># 6#) = singleton '(' <> decimal n' <> singleton ')'
    | otherwise = decimal n'
  where
#if __GLASGOW_HASKELL__ >= 708
    isTrue :: Int# -> Bool
    isTrue b = isTrue# b
#else
    isTrue :: Bool -> Bool
    isTrue = id
#endif

-- | Convert an 'Int8' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbInt8Prec :: Int -> Int8 -> Builder
showbInt8Prec p = showbIntPrec p . fromIntegral
{-# INLINE showbInt8Prec #-}

-- | Convert an 'Int16' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbInt16Prec :: Int -> Int16 -> Builder
showbInt16Prec p = showbIntPrec p . fromIntegral
{-# INLINE showbInt16Prec #-}

-- | Convert an 'Int32' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbInt32Prec :: Int -> Int32 -> Builder
showbInt32Prec p = showbIntPrec p . fromIntegral
{-# INLINE showbInt32Prec #-}

-- | Convert an 'Int64' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbInt64Prec :: Int -> Int64 -> Builder
#if WORD_SIZE_IN_BITS < 64
showbInt64Prec p = showbIntegerPrec p . toInteger
#else
showbInt64Prec p = showbIntPrec p . fromIntegral
#endif
{-# INLINE showbInt64Prec #-}

-- | Convert an 'Integer' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbIntegerPrec :: Int -> Integer -> Builder
showbIntegerPrec p n
    | p > 6 && n < 0 = singleton '(' <> decimal n <> singleton ')'
    | otherwise      = decimal n
{-# INLINE showbIntegerPrec #-}

-- | Convert an 'Integral' type to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbIntegralPrec :: Integral a => Int -> a -> Builder
showbIntegralPrec p = showbIntegerPrec p . toInteger
{-# INLINE showbIntegralPrec #-}

-- | Shows a /non-negative/ 'Integral' number using the base specified by the
-- first argument, and the character representation specified by the second.
--
-- /Since: 2/
showbIntAtBase :: (Integral a, TextShow a) => a -> (Int -> Char) -> a -> Builder
showbIntAtBase base toChr n0
    | base <= 1 = error . toString $ "TextShow.Int.showbIntAtBase: applied to unsupported base" <> showb base
    | n0 < 0    = error . toString $ "TextShow.Int.showbIntAtBase: applied to negative number " <> showb n0
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
        b' = singleton c <> b

-- | Show /non-negative/ 'Integral' numbers in base 2.
--
-- /Since: 2/
showbBin :: (Integral a, TextShow a) => a -> Builder
showbBin = showbIntAtBase 2 intToDigit
{-# INLINE showbBin #-}

-- | Show /non-negative/ 'Integral' numbers in base 16.
--
-- /Since: 2/
showbHex :: (Integral a, TextShow a) => a -> Builder
showbHex = showbIntAtBase 16 intToDigit
{-# INLINE showbHex #-}

-- | Show /non-negative/ 'Integral' numbers in base 8.
--
-- /Since: 2/
showbOct :: (Integral a, TextShow a) => a -> Builder
showbOct = showbIntAtBase 8 intToDigit
{-# INLINE showbOct #-}

-- | Convert a 'Word' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbWord :: Word -> Builder
showbWord = decimal
{-# INLINE showbWord #-}

-- | Convert a 'Word8' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbWord8 :: Word8 -> Builder
showbWord8 = decimal
{-# INLINE showbWord8 #-}

-- | Convert a 'Word16' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbWord16 :: Word16 -> Builder
showbWord16 = decimal
{-# INLINE showbWord16 #-}

-- | Convert a 'Word32' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbWord32 :: Word32 -> Builder
showbWord32 = decimal
{-# INLINE showbWord32 #-}

-- | Convert a 'Word64' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbWord64 :: Word64 -> Builder
showbWord64 = decimal
{-# INLINE showbWord64 #-}

instance TextShow Int where
    showbPrec = showbIntPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow Int8 where
    showbPrec = showbInt8Prec
    INLINE_INST_FUN(showbPrec)

instance TextShow Int16 where
    showbPrec = showbInt16Prec
    INLINE_INST_FUN(showbPrec)

instance TextShow Int32 where
    showbPrec = showbInt32Prec
    INLINE_INST_FUN(showbPrec)

instance TextShow Int64 where
    showbPrec = showbInt64Prec
    INLINE_INST_FUN(showbPrec)

instance TextShow Integer where
    showbPrec = showbIntegerPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow Word where
    showb = showbWord
    INLINE_INST_FUN(showb)

instance TextShow Word8 where
    showb = showbWord8
    INLINE_INST_FUN(showb)

instance TextShow Word16 where
    showb = showbWord16
    INLINE_INST_FUN(showb)

instance TextShow Word32 where
    showb = showbWord32
    INLINE_INST_FUN(showb)

instance TextShow Word64 where
    showb = showbWord64
    INLINE_INST_FUN(showb)
