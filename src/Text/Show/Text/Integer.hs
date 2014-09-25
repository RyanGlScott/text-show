{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Util
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Utilities for showing 'Integer's. This code is adapted from
-- @Text.Show.ByteString.Integer@ from the
-- @<http://hackage.haskell.org/package/bytestring-show bytestring-show>@
-- package.
-- 
-- The @<http://hackage.haskell.org/package/text text>@ package has a 'decimal'
-- function which shows 'Integral' types, but it fails on large 'Integer' values.
-- (See <https://github.com/bos/text/issues/91 this bug>). For this reason, we
-- use this code as a workaround to allow all 'Integer's to be shown.
----------------------------------------------------------------------------
module Text.Show.Text.Integer (showbInteger) where

import Data.Char
import Data.Monoid
import Data.Text.Lazy.Builder

import GHC.Base
#if   __GLASGOW_HASKELL__ && INTEGER_GMP
import GHC.Integer.GMP.Internals
#elif __GLASGOW_HASKELL__ && INTEGER_SIMPLE
import GHC.Integer.Simple.Internals
#endif
import GHC.Num
import GHC.Word

import Text.Show.Text.Util

mx :: Integer
ds :: Int
(mx, ds) = until ((>mi) . (*10) . fst) (\(n,d) -> (n*10,d+1)) (10,1)
 where mi = fromIntegral (maxBound :: Int)

-- | Constructs a 'Builder' from an 'Integer' value.
showbInteger :: Integer -> Builder
#ifdef INTEGER_SIMPLE
#elif INTEGER_GMP
showbInteger (S# i#) = showbI i#
#else
showbInteger (I# i#) = showbI i#
#endif
showbInteger n
  | n < 0     = s '-' <> showbPosInteger (-n)
  | otherwise = showbPosInteger n

showbPosInteger :: Integer -> Builder
showbPosInteger n
  | n < mx    = case fromInteger n of
                  I# i# -> showbI i#
  | otherwise = printh (splitf (mx*mx) n)

splitf :: Integer -> Integer -> [Integer]
splitf p n
  | p > n     = [n]
  | otherwise = splith p (splitf (p*p) n)

splith :: Integer -> [Integer] -> [Integer]
splith _ []     = error "splith: the impossible happened."
splith p (n:ns) = case n `quotRemInteger` p of
#if defined(INTEGER_GMP) || defined(INTEGER_SIMPLE)
  (# q, r #) ->
#else
  (q, r) ->
#endif
          if q > 0
            then q : r : splitb p ns
            else r : splitb p ns

splitb :: Integer -> [Integer] -> [Integer]
splitb _ []     = []
splitb p (n:ns) = case n `quotRemInteger` p of
#if defined(INTEGER_GMP) || defined(INTEGER_SIMPLE)
  (# q, r #) ->
#else
  (q, r) ->
#endif
            q : r : splitb p ns

printh :: [Integer] -> Builder
printh []     = error "printh: the impossible happened."
printh (n:ns) = case n `quotRemInteger` mx of
#if defined(INTEGER_GMP) || defined(INTEGER_SIMPLE)
  (# q', r' #) ->
#else
  (q', r') ->
#endif
              let q = fromInteger q'
                  r = fromInteger r'
              in if q > 0 then bhead q <> bblock r <> printb ns
                          else bhead r <> printb ns

printb :: [Integer] -> Builder
printb []     = mempty
printb (n:ns) = case n `quotRemInteger` mx of
#if defined(INTEGER_GMP) || defined(INTEGER_SIMPLE)
  (# q', r' #) ->
#else
  (q', r') ->
#endif
              let q = fromInteger q'
                  r = fromInteger r'
              in bblock q <> bblock r <> printb ns

bhead :: Int -> Builder
bhead (I# i#) = showbI i#

bblock :: Int -> Builder
bblock = bblock' ds

bblock' :: Int -> Int -> Builder
bblock' d !n
  | d == 1    = unsafeDigitB n
  | otherwise = bblock' (d-1) q <> unsafeDigitB r
 where (q, r) = n `quotRemInt` 10

-- | Puts the decimal digit corresponding to the given Int without
-- checking that it is in the interval [0,9]
unsafeDigitB :: Int -> Builder
unsafeDigitB (I# i#) = unsafeDigitB# (int2Word# i#)

showbWord8 :: Word8 -> Builder
showbWord8 = s . chr . fromIntegral

unsafeDigitB# :: Word# -> Builder
unsafeDigitB# w# = showbWord8 (W8# (w# `plusWord#` int2Word# 48#))

showbI :: Int# -> Builder
showbI i#
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 708
  = case i# <# 0# of
      1#       -> let !(I# minInt#) = minInt
#elif __GLASGOW_HASKELL__
  | i# <# 0#  = let !(I# minInt#) = minInt
#else
  | i# <# 0#  = let I# minInt# = minInt
#endif
                in case i# ==# minInt# of
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 708
                     1# ->
#else
                     True ->
#endif
                        showbWord8 45 <> showbW (int2Word# (negateInt# (i# `quotInt#` 10#)))
                                      <> showbW (int2Word# (negateInt# (i# `remInt#` 10#)))
                     _ ->
                        showbWord8 45 <> showbW (int2Word# (negateInt# i#))
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 708
      _        -> showbW (int2Word# i#)
#else
  | otherwise = showbW (int2Word# i#)
#endif

showbW :: Word# -> Builder
showbW w#
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 708
  = case w# `ltWord#` int2Word# 10# of
      1# ->
#else
  | w# `ltWord#` int2Word# 10# =
#endif
        unsafeDigitB# w#
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 708
      _  ->
#else
  | otherwise =
#endif
        showbW (w# `quotWord#` int2Word# 10#)
        <> unsafeDigitB# (w# `remWord#` int2Word# 10#)