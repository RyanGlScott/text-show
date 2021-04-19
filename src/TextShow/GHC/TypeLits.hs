{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.GHC.TypeLits
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @GHC.TypeLits@ module.

/Since: 2/
-}
module TextShow.GHC.TypeLits () where

import GHC.TypeLits (SomeNat(..), SomeSymbol(..), natVal, symbolVal)

import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(..))
import TextShow.Data.Char ()
import TextShow.Data.Integral ()

-- | /Since: 2/
instance TextShow SomeNat where
    showbPrec p (SomeNat x) = showbPrec p $ natVal x
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow SomeSymbol where
    showb (SomeSymbol x) = showbList $ symbolVal x
    {-# INLINE showb #-}
