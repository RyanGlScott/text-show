{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Integral
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances and monomorphic functions for integral types.

/Since: 2/
-}
module TextShow.Data.Integral (
      showbIntegralPrec
    , showbIntAtBase
    , showbBin
    , showbHex
    , showbOct
    ) where

import           Data.Char (intToDigit)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Text.Lazy.Builder (Builder, singleton)
import           Data.Text.Lazy.Builder.Int (decimal)
import           Data.Word (Word8, Word16, Word32, Word64)

import           GHC.Exts (Int(I#), (<#), (>#))
#if __GLASGOW_HASKELL__ >= 708
import           GHC.Exts (Int#, isTrue#)
#endif

import           Prelude ()
import           Prelude.Compat

import           TextShow.Classes (TextShow(..))
import           TextShow.Utils (toString)

-- | Convert an 'Integral' type to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbIntegralPrec :: Integral a => Int -> a -> Builder
showbIntegralPrec p = showbPrec p . toInteger
{-# INLINE showbIntegralPrec #-}

-- | Shows a /non-negative/ 'Integral' number using the base specified by the
-- first argument, and the character representation specified by the second.
--
-- /Since: 2/
showbIntAtBase :: (Integral a, TextShow a) => a -> (Int -> Char) -> a -> Builder
{-# SPECIALIZE showbIntAtBase :: Int     -> (Int -> Char) -> Int     -> Builder #-}
{-# SPECIALIZE showbIntAtBase :: Int8    -> (Int -> Char) -> Int8    -> Builder #-}
{-# SPECIALIZE showbIntAtBase :: Int16   -> (Int -> Char) -> Int16   -> Builder #-}
{-# SPECIALIZE showbIntAtBase :: Int32   -> (Int -> Char) -> Int32   -> Builder #-}
{-# SPECIALIZE showbIntAtBase :: Int64   -> (Int -> Char) -> Int64   -> Builder #-}
{-# SPECIALIZE showbIntAtBase :: Integer -> (Int -> Char) -> Integer -> Builder #-}
{-# SPECIALIZE showbIntAtBase :: Word    -> (Int -> Char) -> Word    -> Builder #-}
{-# SPECIALIZE showbIntAtBase :: Word8   -> (Int -> Char) -> Word8   -> Builder #-}
{-# SPECIALIZE showbIntAtBase :: Word16  -> (Int -> Char) -> Word16  -> Builder #-}
{-# SPECIALIZE showbIntAtBase :: Word32  -> (Int -> Char) -> Word32  -> Builder #-}
{-# SPECIALIZE showbIntAtBase :: Word64  -> (Int -> Char) -> Word64  -> Builder #-}
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

-- | /Since: 2/
instance TextShow Int where
    showbPrec (I# p) n'@(I# n)
        | isTrue (n <# 0#) && isTrue (p ># 6#)
        = singleton '(' <> decimal n' <> singleton ')'
        | otherwise
        = decimal n'
      where
#if __GLASGOW_HASKELL__ >= 708
        isTrue :: Int# -> Bool
        isTrue b = isTrue# b
#else
        isTrue :: Bool -> Bool
        isTrue = id
#endif

-- | /Since: 2/
instance TextShow Int8 where
    showbPrec p x = showbPrec p (fromIntegral x :: Int)
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow Int16 where
    showbPrec p x = showbPrec p (fromIntegral x :: Int)
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow Int32 where
    showbPrec p x = showbPrec p (fromIntegral x :: Int)
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow Int64 where
#if WORD_SIZE_IN_BITS < 64
    showbPrec p   = showbPrec p . toInteger
#else
    showbPrec p x = showbPrec p (fromIntegral x :: Int)
#endif
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow Integer where
    showbPrec p n
        | p > 6 && n < 0 = singleton '(' <> decimal n <> singleton ')'
        | otherwise      = decimal n
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow Word where
    showb = decimal
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow Word8 where
    showb = decimal
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow Word16 where
    showb = decimal
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow Word32 where
    showb = decimal
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow Word64 where
    showb = decimal
    {-# INLINE showb #-}
