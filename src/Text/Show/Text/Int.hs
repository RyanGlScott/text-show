{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Show.Text.Int where

import           Data.Char (intToDigit)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Monoid ((<>), mempty)
import           Data.Text.Buildable (build)
import           Data.Text.Lazy.Builder (Builder)
import           Data.Word (Word, Word8, Word16, Word32, Word64)

import           GHC.Exts (Int(I#))
#if __GLASGOW_HASKELL__ >= 708
import           GHC.Exts (isTrue#)
#endif
import           GHC.Prim (Int#, (<#), (>#))

import qualified Prelude as P (show)
import           Prelude hiding (Show)

import           Text.Show.Text.Class (Show(..))
import           Text.Show.Text.Functions (s)

showbInt :: Int -> Int -> Builder
showbInt (I# p) n'@(I# n)
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

showbInt64 :: Int -> Int64 -> Builder
#if WORD_SIZE_IN_BITS < 64
showbInt64 p = showbInteger p . toInteger
#else
showbInt64 p x = showbInt p (fromIntegral x :: Int)
#endif
{-# INLINE showbInt64 #-}

showbInteger :: Int -> Integer -> Builder
showbInteger p n
    | p > 6 && n < 0 = s '(' <> build n <> s ')'
    | otherwise      = build n
{-# INLINE showbInteger #-}

showbIntegral :: Integral a => Int -> a -> Builder
showbIntegral p = showbInteger p . toInteger
{-# INLINE showbIntegral #-}

-- | Shows a /non-negative/ 'Integral' number using the base specified by the
-- first argument, and the character representation specified by the second.
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

-- | Show /non-negative/ 'Integral' numbers in base 16.
showbHex :: (Integral a, Show a) => a -> Builder
showbHex = showbIntAtBase 16 intToDigit
{-# INLINE showbHex #-}

instance Show Int where
    showbPrec = showbInt
    {-# INLINE showbPrec #-}

instance Show Int8 where
    showbPrec p x = showbPrec p (fromIntegral x :: Int)
    {-# INLINE showbPrec #-}

instance Show Int16 where
    showbPrec p x = showbPrec p (fromIntegral x :: Int)
    {-# INLINE showbPrec #-}

instance Show Int32 where
    showbPrec p x = showbPrec p (fromIntegral x :: Int)
    {-# INLINE showbPrec #-}

instance Show Int64 where
    showbPrec = showbInt64
    {-# INLINE showbPrec #-}

instance Show Integer where
    showbPrec = showbInteger
    {-# INLINE showbPrec #-}

instance Show Word where
    showb = build
    {-# INLINE showb #-}

instance Show Word8 where
    showb = build
    {-# INLINE showb #-}

instance Show Word16 where
    showb = build
    {-# INLINE showb #-}

instance Show Word32 where
    showb = build
    {-# INLINE showb #-}

instance Show Word64 where
    showb = build
    {-# INLINE showb #-}