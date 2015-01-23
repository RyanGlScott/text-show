{-# LANGUAGE CPP, MagicHash, OverloadedStrings #-}
#if !defined(RECENT_TEXT)
{-# LANGUAGE BangPatterns, RankNTypes, ScopedTypeVariables, UnboxedTuples #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Integral
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for integral types.

/Since: 0.3/
-}
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
    , showbWord
    , showbWord8
    , showbWord16
    , showbWord32
    , showbWord64
    ) where

import           Data.Char (intToDigit)
import           Data.Int (Int8, Int16, Int32, Int64)
#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (mempty)
#endif
import           Data.Text.Lazy.Builder (Builder)
import           Data.Word ( Word8
                           , Word16
                           , Word32
                           , Word64
#if !(MIN_VERSION_base(4,8,0))
                           , Word
#endif
                           )

import           GHC.Exts (Int(I#))
#if __GLASGOW_HASKELL__ >= 708
import           GHC.Exts (isTrue#)
import           GHC.Prim (Int#)
#endif
import           GHC.Prim ((<#), (>#))

import           Prelude hiding (Show)

import           Text.Show.Text.Classes (Show(showb, showbPrec))
import           Text.Show.Text.Utils ((<>), s, toString)

#if defined(RECENT_TEXT)
import           Data.Text.Lazy.Builder.Int (decimal)
#else
import           Control.Monad.ST (ST)

import qualified Data.ByteString.Unsafe as B
import           Data.Text.Array (MArray, unsafeWrite)
import           Data.Text.Internal.Builder (writeN)
import           Data.Text.Internal.Builder.Int.Digits (digits)

import           GHC.Base (quotInt, remInt)

# if defined(INTEGER_GMP)
import           GHC.Integer.GMP.Internals (Integer(..))
# elif defined(INTEGER_SIMPLE)
import           GHC.Integer (Integer(..))
# else
# error "You need to use either GMP or integer-simple."
# endif

import           GHC.Num (quotRemInteger)

import           Text.Show.Text.Utils (i2d)
#endif

#include "inline.h"

#if defined(INTEGER_GMP) || defined(INTEGER_SIMPLE)
# define PAIR(a,b) (# a,b #)
#else
# define PAIR(a,b) (a,b)
#endif

-- | Convert an 'Int' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbIntPrec :: Int -> Int -> Builder
showbIntPrec (I# p) n'@(I# n)
    | isTrue (n <# 0#) && isTrue (p ># 6#) = s '(' <> decimal n' <> s ')'
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
-- /Since: 0.3/
showbInt8Prec :: Int -> Int8 -> Builder
showbInt8Prec p = showbIntPrec p . fromIntegral
{-# INLINE showbInt8Prec #-}

-- | Convert an 'Int16' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbInt16Prec :: Int -> Int16 -> Builder
showbInt16Prec p = showbIntPrec p . fromIntegral
{-# INLINE showbInt16Prec #-}

-- | Convert an 'Int32' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbInt32Prec :: Int -> Int32 -> Builder
showbInt32Prec p = showbIntPrec p . fromIntegral
{-# INLINE showbInt32Prec #-}

-- | Convert an 'Int64' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbInt64Prec :: Int -> Int64 -> Builder
#if WORD_SIZE_IN_BITS < 64
showbInt64Prec p = showbIntegerPrec p . toInteger
#else
showbInt64Prec p = showbIntPrec p . fromIntegral
#endif
{-# INLINE showbInt64Prec #-}

-- | Convert an 'Integer' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbIntegerPrec :: Int -> Integer -> Builder
showbIntegerPrec p n
    | p > 6 && n < 0 = s '(' <> decimal n <> s ')'
    | otherwise      = decimal n
{-# INLINE showbIntegerPrec #-}

-- | Convert an 'Integral' type to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbIntegralPrec :: Integral a => Int -> a -> Builder
showbIntegralPrec p = showbIntegerPrec p . toInteger
{-# INLINE showbIntegralPrec #-}

-- | Shows a /non-negative/ 'Integral' number using the base specified by the
-- first argument, and the character representation specified by the second.
-- 
-- /Since: 0.3/
showbIntAtBase :: (Integral a, Show a) => a -> (Int -> Char) -> a -> Builder
showbIntAtBase base toChr n0
    | base <= 1 = error . toString $ "Text.Show.Text.Int.showbIntAtBase: applied to unsupported base" <> showb base
    | n0 < 0    = error . toString $ "Text.Show.Text.Int.showbIntAtBase: applied to negative number " <> showb n0
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

-- | Show /non-negative/ 'Integral' numbers in base 2.
-- 
-- /Since: 0.3/
showbBin :: (Integral a, Show a) => a -> Builder
showbBin = showbIntAtBase 2 intToDigit
{-# INLINE showbBin #-}

-- | Show /non-negative/ 'Integral' numbers in base 16.
-- 
-- /Since: 0.3/
showbHex :: (Integral a, Show a) => a -> Builder
showbHex = showbIntAtBase 16 intToDigit
{-# INLINE showbHex #-}

-- | Show /non-negative/ 'Integral' numbers in base 8.
-- 
-- /Since: 0.3/
showbOct :: (Integral a, Show a) => a -> Builder
showbOct = showbIntAtBase 8 intToDigit
{-# INLINE showbOct #-}

-- | Convert a 'Word' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbWord :: Word -> Builder
showbWord = decimal
{-# INLINE showbWord #-}

-- | Convert a 'Word8' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbWord8 :: Word8 -> Builder
showbWord8 = decimal
{-# INLINE showbWord8 #-}

-- | Convert a 'Word16' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbWord16 :: Word16 -> Builder
showbWord16 = decimal
{-# INLINE showbWord16 #-}

-- | Convert a 'Word32' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbWord32 :: Word32 -> Builder
showbWord32 = decimal
{-# INLINE showbWord32 #-}

-- | Convert a 'Word64' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbWord64 :: Word64 -> Builder
showbWord64 = decimal
{-# INLINE showbWord64 #-}

#if !defined(RECENT_TEXT)
decimal :: Integral a => a -> Builder
{-# RULES "decimal/Int8" decimal = boundedDecimal :: Int8 -> Builder #-}
{-# RULES "decimal/Int" decimal = boundedDecimal :: Int -> Builder #-}
{-# RULES "decimal/Int16" decimal = boundedDecimal :: Int16 -> Builder #-}
{-# RULES "decimal/Int32" decimal = boundedDecimal :: Int32 -> Builder #-}
{-# RULES "decimal/Int64" decimal = boundedDecimal :: Int64 -> Builder #-}
{-# RULES "decimal/Word" decimal = positive :: Word -> Builder #-}
{-# RULES "decimal/Word8" decimal = positive :: Word8 -> Builder #-}
{-# RULES "decimal/Word16" decimal = positive :: Word16 -> Builder #-}
{-# RULES "decimal/Word32" decimal = positive :: Word32 -> Builder #-}
{-# RULES "decimal/Word64" decimal = positive :: Word64 -> Builder #-}
{-# RULES "decimal/Integer" decimal = integer 10 :: Integer -> Builder #-}
decimal i = decimal' (<= -128) i
{-# NOINLINE decimal #-}

boundedDecimal :: (Integral a, Bounded a) => a -> Builder
{-# SPECIALIZE boundedDecimal :: Int -> Builder #-}
{-# SPECIALIZE boundedDecimal :: Int8 -> Builder #-}
{-# SPECIALIZE boundedDecimal :: Int16 -> Builder #-}
{-# SPECIALIZE boundedDecimal :: Int32 -> Builder #-}
{-# SPECIALIZE boundedDecimal :: Int64 -> Builder #-}
boundedDecimal i = decimal' (== minBound) i

decimal' :: Integral a => (a -> Bool) -> a -> Builder
{-# INLINE decimal' #-}
decimal' p i
    | i < 0 = if p i
              then let (q, r) = i `quotRem` 10
                       qq = -q
                       !n = countDigits qq
                   in writeN (n + 2) $ \marr off -> do
                       unsafeWrite marr off minus
                       posDecimal marr (off+1) n qq
                       unsafeWrite marr (off+n+1) (i2w (-r))
              else let j = -i
                       !n = countDigits j
                   in writeN (n + 1) $ \marr off ->
                       unsafeWrite marr off minus >> posDecimal marr (off+1) n j
    | otherwise = positive i

positive :: Integral a => a -> Builder
{-# SPECIALIZE positive :: Int -> Builder #-}
{-# SPECIALIZE positive :: Int8 -> Builder #-}
{-# SPECIALIZE positive :: Int16 -> Builder #-}
{-# SPECIALIZE positive :: Int32 -> Builder #-}
{-# SPECIALIZE positive :: Int64 -> Builder #-}
{-# SPECIALIZE positive :: Word -> Builder #-}
{-# SPECIALIZE positive :: Word8 -> Builder #-}
{-# SPECIALIZE positive :: Word16 -> Builder #-}
{-# SPECIALIZE positive :: Word32 -> Builder #-}
{-# SPECIALIZE positive :: Word64 -> Builder #-}
positive i
    | i < 10    = writeN 1 $ \marr off -> unsafeWrite marr off (i2w i)
    | otherwise = let !n = countDigits i
                  in writeN n $ \marr off -> posDecimal marr off n i

posDecimal :: Integral a =>
              forall s. MArray s -> Int -> Int -> a -> ST s ()
{-# INLINE posDecimal #-}
posDecimal marr off0 ds v0 = go (off0 + ds - 1) v0
  where go off v
           | v >= 100 = do
               let (q, r) = v `quotRem` 100
               write2 off r
               go (off - 2) q
           | v < 10    = unsafeWrite marr off (i2w v)
           | otherwise = write2 off v
        write2 off i0 = do
          let i = fromIntegral i0; j = i + i
          unsafeWrite marr off $ get (j + 1)
          unsafeWrite marr (off - 1) $ get j
        get = fromIntegral . B.unsafeIndex digits

minus, zero :: Word16
{-# INLINE minus #-}
{-# INLINE zero #-}
minus = 45
zero = 48

i2w :: Integral a => a -> Word16
{-# INLINE i2w #-}
i2w v = zero + fromIntegral v

countDigits :: Integral a => a -> Int
{-# INLINE countDigits #-}
countDigits v0
  | fromIntegral v64 == v0 = go 1 v64
  | otherwise              = goBig 1 (fromIntegral v0)
  where v64 = fromIntegral v0
        goBig !k (v :: Integer)
           | v > big   = goBig (k + 19) (v `quot` big)
           | otherwise = go k (fromIntegral v)
        big = 10000000000000000000
        go !k (v :: Word64)
           | v < 10    = k
           | v < 100   = k + 1
           | v < 1000  = k + 2
           | v < 1000000000000 =
               k + if v < 100000000
                   then if v < 1000000
                        then if v < 10000
                             then 3
                             else 4 + fin v 100000
                        else 6 + fin v 10000000
                   else if v < 10000000000
                        then 8 + fin v 1000000000
                        else 10 + fin v 100000000000
           | otherwise = go (k + 12) (v `quot` 1000000000000)
        fin v n = if v >= n then 1 else 0

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
{-# RULES "hexadecimal/Integer"
    hexadecimal = hexInteger :: Integer -> Builder #-}
hexadecimal i
    | i < 0     = error hexErrMsg
    | otherwise = go i
  where
    go n | n < 16    = hexDigit n
         | otherwise = go (n `quot` 16) <> hexDigit (n `rem` 16)
{-# NOINLINE[0] hexadecimal #-}

hexInteger :: Integer -> Builder
hexInteger i
    | i < 0     = error hexErrMsg
    | otherwise = integer 16 i

hexErrMsg :: String
hexErrMsg = "Data.Text.Lazy.Builder.Int.hexadecimal: applied to negative number"

hexDigit :: Integral a => a -> Builder
hexDigit n
    | n <= 9    = s $! i2d (fromIntegral n)
    | otherwise = s $! toEnum (fromIntegral n + 87)
{-# INLINE hexDigit #-}

data T = T !Integer !Int

integer :: Int -> Integer -> Builder
integer 10 (S# i#) = decimal (I# i#)
integer 16 (S# i#) = hexadecimal (I# i#)
integer base i
    | i < 0     = s '-' <> go (-i)
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

    int :: Int -> Builder
    int x | base == 10 = decimal x
          | otherwise  = hexadecimal x

    pblock = loop maxDigits
      where
        loop !d !n
            | d == 1    = hexDigit n
            | otherwise = loop (d-1) q <> hexDigit r
            where q = n `quotInt` base
                  r = n `remInt` base
#endif

instance Show Int where
    showbPrec = showbIntPrec
    INLINE_INST_FUN(showbPrec)

instance Show Int8 where
    showbPrec = showbInt8Prec
    INLINE_INST_FUN(showbPrec)

instance Show Int16 where
    showbPrec = showbInt16Prec
    INLINE_INST_FUN(showbPrec)

instance Show Int32 where
    showbPrec = showbInt32Prec
    INLINE_INST_FUN(showbPrec)

instance Show Int64 where
    showbPrec = showbInt64Prec
    INLINE_INST_FUN(showbPrec)

instance Show Integer where
    showbPrec = showbIntegerPrec
    INLINE_INST_FUN(showbPrec)

instance Show Word where
    showb = showbWord
    INLINE_INST_FUN(showb)

instance Show Word8 where
    showb = showbWord8
    INLINE_INST_FUN(showb)

instance Show Word16 where
    showb = showbWord16
    INLINE_INST_FUN(showb)

instance Show Word32 where
    showb = showbWord32
    INLINE_INST_FUN(showb)

instance Show Word64 where
    showb = showbWord64
    INLINE_INST_FUN(showb)
