{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Fixed
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Fixed' values.
-}
module Text.Show.Text.Data.Fixed (showbFixed) where

import Data.Fixed (HasResolution(..))
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb))

#if MIN_VERSION_base(4,7,0)
import Data.Fixed (Fixed(..))
import Data.Int (Int64)
import Data.Monoid (mempty)

import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Utils ((<>), lengthB, replicateB, s)
#else
import Data.Fixed (Fixed, showFixed)
import Data.Text.Lazy.Builder (fromString)
#endif

-- | Convert a 'Fixed' value to a 'Builder', where the first argument indicates
-- whether to chop off trailing zeroes.
showbFixed :: HasResolution a => Bool -> Fixed a -> Builder
#if MIN_VERSION_base(4,7,0)
showbFixed chopTrailingZeroes fa@(MkFixed a) | a < 0
    = s '-' <> showbFixed chopTrailingZeroes (asTypeOf (MkFixed (negate a)) fa)
showbFixed chopTrailingZeroes fa@(MkFixed a)
    = showb i <> withDotB (showbIntegerZeroes chopTrailingZeroes digits fracNum)
  where
    res     = fromInteger $ resolution fa
    (i, d)  = divMod (fromInteger a) res
    digits  = ceiling (logBase 10 (fromInteger $ resolution fa) :: Double)
    maxnum  = 10 ^ digits
    fracNum = div (d * maxnum) res
#else
showbFixed chopTrailingZeroes = fromString . showFixed chopTrailingZeroes
#endif
{-# INLINE showbFixed #-}

#if MIN_VERSION_base(4,7,0)
-- | Only works for positive 'Integer's.
showbIntegerZeroes :: Bool -> Int64 -> Integer -> Builder
showbIntegerZeroes True _ 0 = mempty
showbIntegerZeroes chopTrailingZeroes digits a = replicateB (digits - lengthB sh) (s '0') <> sh'
  where
    sh, sh' :: Builder
    sh  = showb a
    sh' = if chopTrailingZeroes then chopZeroesB a else sh
{-# INLINE showbIntegerZeroes #-}

-- | Chops off the trailing zeroes of an 'Integer'.
chopZeroesB :: Integer -> Builder
chopZeroesB 0 = mempty
chopZeroesB a | mod a 10 == 0 = chopZeroesB (div a 10)
chopZeroesB a = showb a
{-# INLINE chopZeroesB #-}

-- | Prepends a dot to a non-empty 'Builder'.
withDotB :: Builder -> Builder
withDotB b | b == mempty = mempty
           | otherwise   = s '.' <> b
{-# INLINE withDotB #-}
#endif

instance HasResolution a => Show (Fixed a) where
    showb = showbFixed False
    {-# INLINE showb #-}