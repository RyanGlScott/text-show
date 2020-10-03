{-# LANGUAGE CPP       #-}
#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE MagicHash #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import GHC.Num (integerFromNatural)
import GHC.Num.Natural (Natural(..))
#elif MIN_VERSION_base(4,8,0)
import GHC.Integer.GMP.Internals (Integer(..))
import GHC.Natural (Natural(..))
#else
import Numeric.Natural.Compat (Natural)
#endif

#if MIN_VERSION_base(4,8,0)
import GHC.Exts (Word(..))
#endif

import TextShow.Classes (TextShow(..))
import TextShow.Data.Integral ()

-- | /Since: 2/
instance TextShow Natural where
#if MIN_VERSION_base(4,15,0)
    showbPrec p (NS w) = showbPrec p (W# w)
    showbPrec p n      = showbPrec p (integerFromNatural n)
#elif MIN_VERSION_base(4,8,0)
    showbPrec _ (NatS# w#)  = showb $ W# w#
    showbPrec p (NatJ# bn)  = showbPrec p $ Jp# bn
#else
    showbPrec p             = showbPrec p . toInteger
    {-# INLINE showbPrec #-}
#endif
