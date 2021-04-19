{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Fixed
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'TextShow' instance for 'Fixed', as well as the 'showbFixed' function.

/Since: 2/
-}
module TextShow.Data.Fixed (showbFixed) where

import Data.Fixed (Fixed(..), HasResolution(..))
import Data.Int (Int64)
import Data.Semigroup.Compat (mtimesDefault)
import Data.Text.Lazy.Builder (Builder, singleton)

import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(..))
import TextShow.Data.Integral ()
import TextShow.Utils (lengthB)

#if MIN_VERSION_base(4,13,0)
import TextShow.Classes (showbParen)
#endif

-- | Convert a 'Fixed' value to a 'Builder', where the first argument indicates
-- whether to chop off trailing zeroes.
--
-- /Since: 2/
showbFixed :: HasResolution a => Bool -> Fixed a -> Builder
showbFixed chopTrailingZeroes fa@(MkFixed a) | a < 0
    = singleton '-' <> showbFixed chopTrailingZeroes (asTypeOf (MkFixed (negate a)) fa)
showbFixed chopTrailingZeroes fa@(MkFixed a)
    = showb i <> withDotB (showbIntegerZeroes chopTrailingZeroes digits fracNum)
  where
    res     = fromInteger $ resolution fa
    (i, d)  = divMod (fromInteger a) res
    digits  = ceiling (logBase 10 (fromInteger $ resolution fa) :: Double)
    maxnum  = 10 ^ digits
#if MIN_VERSION_base(4,8,0)
    fracNum = divCeil (d * maxnum) res
    divCeil x y = (x + y - 1) `div` y
#else
    fracNum = div (d * maxnum) res
#endif

-- | Only works for positive 'Integer's.
showbIntegerZeroes :: Bool -> Int64 -> Integer -> Builder
showbIntegerZeroes True _ 0 = mempty
showbIntegerZeroes chopTrailingZeroes digits a
    = mtimesDefault (max 0 $ digits - lengthB sh) (singleton '0') <> sh'
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

-- | /Since: 2/
instance HasResolution a => TextShow (Fixed a) where
#if MIN_VERSION_base(4,13,0)
    showbPrec p n = showbParen (p > 6 && n < 0) $ showbFixed False n
#else
    showb = showbFixed False
    {-# INLINE showb #-}
#endif
