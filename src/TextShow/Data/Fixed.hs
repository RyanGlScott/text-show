{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Fixed
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' function for 'Fixed' values.

/Since: 2/
-}
module TextShow.Data.Fixed (showbFixed) where

import Data.Fixed (HasResolution(..))
import Data.Text.Lazy.Builder (Builder)

import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(showb))

#if MIN_VERSION_base(4,7,0)
import Data.Fixed (Fixed(..))
import Data.Int (Int64)
import Data.Monoid.Compat ((<>))
import Data.Semigroup (timesN)
import Data.Text.Lazy.Builder (singleton)

import TextShow.Data.Integral ()
import TextShow.Utils (lengthB)
#else
import Data.Fixed (Fixed, showFixed)
import Data.Text.Lazy.Builder (fromString)
#endif

#include "inline.h"

-- | Convert a 'Fixed' value to a 'Builder', where the first argument indicates
-- whether to chop off trailing zeroes.
--
-- /Since: 2/
showbFixed :: HasResolution a => Bool -> Fixed a -> Builder
#if MIN_VERSION_base(4,7,0)
showbFixed chopTrailingZeroes fa@(MkFixed a) | a < 0
    = singleton '-' <> showbFixed chopTrailingZeroes (asTypeOf (MkFixed (negate a)) fa)
showbFixed chopTrailingZeroes fa@(MkFixed a)
    = showb i <> withDotB (showbIntegerZeroes chopTrailingZeroes digits fracNum)
  where
    res     = fromInteger $ resolution fa
    (i, d)  = divMod (fromInteger a) res
    digits  = ceiling (logBase 10 (fromInteger $ resolution fa) :: Double)
    maxnum  = 10 ^ digits
# if MIN_VERSION_base(4,8,0)
    fracNum = divCeil (d * maxnum) res
    divCeil x y = (x + y - 1) `div` y
# else
    fracNum = div (d * maxnum) res
# endif
#else
showbFixed chopTrailingZeroes = fromString . showFixed chopTrailingZeroes
{-# INLINE showbFixed #-}
#endif

#if MIN_VERSION_base(4,7,0)
-- | Only works for positive 'Integer's.
showbIntegerZeroes :: Bool -> Int64 -> Integer -> Builder
showbIntegerZeroes True _ 0 = mempty
showbIntegerZeroes chopTrailingZeroes digits a
    = timesN (fromIntegral . max 0 $ digits - lengthB sh) (singleton '0') <> sh'
  where
    sh, sh' :: Builder
    sh  = showb a
    sh' = if chopTrailingZeroes then chopZeroesB a else sh

-- | Chops off the trailing zeroes of an 'Integer'.
chopZeroesB :: Integer -> Builder
chopZeroesB 0 = mempty
chopZeroesB a | mod a 10 == 0 = chopZeroesB (div a 10)
chopZeroesB a = showb a

-- | Prepends a dot to a non-empty 'Builder'.
withDotB :: Builder -> Builder
withDotB b | b == mempty = mempty
           | otherwise   = singleton '.' <> b
{-# INLINE withDotB #-}
#endif

instance HasResolution a => TextShow (Fixed a) where
    showb = showbFixed False
    INLINE_INST_FUN(showb)
