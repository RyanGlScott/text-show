{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude, OverloadedStrings #-}
#if !defined(TEXT_FORMAT) && defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 611
{-# LANGUAGE BangPatterns, UnboxedTuples #-}
#endif
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
import           Data.Monoid (mempty)
import           Data.Ratio (Ratio, numerator, denominator)
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

import           Text.Show.Text.Class (Show(showb, showbPrec), showbParen)
import           Text.Show.Text.Utils ((<>), s)

#if defined(TEXT_FORMAT)
import           Data.Text.Buildable (build)
#else
import           GHC.Base (quotInt, remInt)
import           GHC.Num (quotRemInteger)
import           Text.Show.Text.Utils (i2d)

# if defined(__GLASGOW_HASKELL__)
#  if __GLASGOW_HASKELL__ < 611

import GHC.Integer.Internals
#define PAIR(a,b) (a,b)

#  else

import GHC.Integer.GMP.Internals
#define PAIR(a,b) (# a,b #)

#  endif
# endif
#endif

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

#if !defined(TEXT_FORMAT)
build :: Integral a => a -> Builder
build = decimal
{-# INLINE build #-}

decimal :: Integral a => a -> Builder
{-# SPECIALIZE decimal :: Int -> Builder #-}
{-# SPECIALIZE decimal :: Int8 -> Builder #-}
{-# SPECIALIZE decimal :: Int16 -> Builder #-}
{-# SPECIALIZE decimal :: Int32 -> Builder #-}
{-# SPECIALIZE decimal :: Int64 -> Builder #-}
{-# SPECIALIZE decimal :: Word -> Builder #-}
{-# SPECIALIZE decimal :: Word8 -> Builder #-}
{-# SPECIALIZE decimal :: Word16 -> Builder #-}
{-# SPECIALIZE decimal :: Word32 -> Builder #-}
{-# SPECIALIZE decimal :: Word64 -> Builder #-}
{-# RULES "decimal/Integer" decimal = integer 10 :: Integer -> Builder #-}
decimal i
    | i < 0     = minus <> go (-i)
    | otherwise = go i
  where
    go n | n < 10    = digit n
         | otherwise = go (n `quot` 10) <> digit (n `rem` 10)
{-# NOINLINE[0] decimal #-}

hexadecimal :: Integral a => a -> Builder
{-# SPECIALIZE hexadecimal :: Int -> Builder #-}
{-# SPECIALIZE hexadecimal :: Int8 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Int16 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Int32 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Int64 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word8 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word16 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word32 -> Builder #-}
{-# SPECIALIZE hexadecimal :: Word64 -> Builder #-}
{-# RULES "hexadecimal/Integer" hexadecimal = integer 16 :: Integer -> Builder #-}
hexadecimal i
    | i < 0     = minus <> go (-i)
    | otherwise = go i
  where
    go n | n < 16    = hexDigit n
         | otherwise = go (n `quot` 16) <> hexDigit (n `rem` 16)
{-# NOINLINE[0] hexadecimal #-}

digit :: Integral a => a -> Builder
digit n = s $! i2d (fromIntegral n)
{-# INLINE digit #-}

hexDigit :: Integral a => a -> Builder
hexDigit n
    | n <= 9    = s $! i2d (fromIntegral n)
    | otherwise = s $! toEnum (fromIntegral n + 87)
{-# INLINE hexDigit #-}

minus :: Builder
minus = s '-'
{-# INLINE minus #-}

int :: Int -> Builder
int = decimal
{-# INLINE int #-}

data T = T !Integer !Int

integer :: Int -> Integer -> Builder
integer 10 (S# i#) = decimal (I# i#)
integer 16 (S# i#) = hexadecimal (I# i#)
integer base i
    | i < 0     = minus <> go (-i)
    | otherwise = go i
  where
    go n | n < maxInt = int (fromInteger n)
         | otherwise  = putH (splitf (maxInt * maxInt) n)

    splitf p n
      | p > n       = [n]
      | otherwise   = splith p (splitf (p*p) n)

    splith p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) | q > 0     -> q : r : splitb p ns
                                  | otherwise -> r : splitb p ns
    splith _ _      = error "splith: the impossible happened."

    splitb p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) -> q : r : splitb p ns
    splitb _ _      = []

    T maxInt10 maxDigits10 =
        until ((>mi) . (*10) . fstT) (\(T n d) -> T (n*10) (d+1)) (T 10 1)
      where mi = fromIntegral (maxBound :: Int)
    T maxInt16 maxDigits16 =
        until ((>mi) . (*16) . fstT) (\(T n d) -> T (n*16) (d+1)) (T 16 1)
      where mi = fromIntegral (maxBound :: Int)

    fstT (T a _) = a

    maxInt | base == 10 = maxInt10
           | otherwise  = maxInt16
    maxDigits | base == 10 = maxDigits10
              | otherwise  = maxDigits16

    putH (n:ns) = case n `quotRemInteger` maxInt of
                    PAIR(x,y)
                        | q > 0     -> int q <> pblock r <> putB ns
                        | otherwise -> int r <> putB ns
                        where q = fromInteger x
                              r = fromInteger y
    putH _ = error "putH: the impossible happened"

    putB (n:ns) = case n `quotRemInteger` maxInt of
                    PAIR(x,y) -> pblock q <> pblock r <> putB ns
                        where q = fromInteger x
                              r = fromInteger y
    putB _ = mempty

    pblock = loop maxDigits
      where
        loop !d !n
            | d == 1    = digit n
            | otherwise = loop (d-1) q <> digit r
            where q = n `quotInt` base
                  r = n `remInt` base
#endif

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