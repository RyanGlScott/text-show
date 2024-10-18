{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Numeric.Natural
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Natural'.

/Since: 2/
-}
module TextShow.Numeric.Natural () where

#if MIN_VERSION_base(4,15,0)
import GHC.Exts (Word(..))
import GHC.Num (integerFromNatural)
import GHC.Num.Natural (Natural(..))
#elif defined(MIN_VERSION_integer_gmp)
import GHC.Exts (Word(..))
import GHC.Integer.GMP.Internals (Integer(..))
import GHC.Natural (Natural(..))
#else
import Numeric.Natural (Natural)
#endif

import TextShow.Classes (TextShow(..))
import TextShow.Data.Integral ()

-- | /Since: 2/
instance TextShow Natural where
#if MIN_VERSION_base(4,15,0)
    showbPrec p (NS w) = showbPrec p (W# w)
    showbPrec p n      = showbPrec p (integerFromNatural n)
#elif defined(MIN_VERSION_integer_gmp)
    showbPrec _ (NatS# w#)  = showb $ W# w#
    showbPrec p (NatJ# bn)  = showbPrec p $ Jp# bn
#else
    showbPrec p             = showbPrec p . toInteger
    {-# INLINE showbPrec #-}
#endif
