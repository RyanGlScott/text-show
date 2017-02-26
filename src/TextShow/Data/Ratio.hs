{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Ratio
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Ratio'.

/Since: 2/
-}
module TextShow.Data.Ratio () where

import Data.Monoid.Compat ((<>))

import GHC.Real (Ratio(..), ratioPrec, ratioPrec1)

import TextShow.Classes (TextShow(..), showbParen)
#if MIN_VERSION_base(4,4,0)
import TextShow.Classes (TextShow1(..))
#endif
import TextShow.Data.Integral ()

-- | Note that on @base-4.3.0.0@, this must have a @('TextShow' a, 'Integral' a)@
-- constraint instead of just a @('TextShow' a)@ constraint.
--
-- /Since: 2/
instance
#if MIN_VERSION_base(4,4,0)
         TextShow a
#else
         (TextShow a, Integral a)
#endif
      => TextShow (Ratio a) where
    {-# SPECIALIZE instance TextShow Rational #-}
    showbPrec p (numer :% denom) = showbParen (p > ratioPrec) $
           showbPrec ratioPrec1 numer
        <> " % "
        <> showbPrec ratioPrec1 denom
    {-# INLINE showbPrec #-}

#if MIN_VERSION_base(4,4,0)
-- | Only available with @base-4.4.0.0@ or later.
--
-- /Since: 2/
instance TextShow1 Ratio where
    liftShowbPrec sp _ p (numer :% denom) = showbParen (p > ratioPrec) $
           sp ratioPrec1 numer
        <> " % "
        <> sp ratioPrec1 denom
    {-# INLINE liftShowbPrec #-}
#endif
