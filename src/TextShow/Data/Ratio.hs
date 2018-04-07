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

import GHC.Real (Ratio(..), ratioPrec, ratioPrec1)

import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(..), TextShow1(..), showbParen)
import TextShow.Data.Integral ()

-- | /Since: 2/
instance TextShow a => TextShow (Ratio a) where
    {-# SPECIALIZE instance TextShow Rational #-}
    showbPrec p (numer :% denom) = showbParen (p > ratioPrec) $
           showbPrec ratioPrec1 numer
        <> " % "
        <> showbPrec ratioPrec1 denom
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 Ratio where
    liftShowbPrec sp _ p (numer :% denom) = showbParen (p > ratioPrec) $
           sp ratioPrec1 numer
        <> " % "
        <> sp ratioPrec1 denom
    {-# INLINE liftShowbPrec #-}
