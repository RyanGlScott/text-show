{-# LANGUAGE CPP #-}
#if !(MIN_VERSION_base(4,7,0))
{-# LANGUAGE FlexibleContexts, GADTs, KindSignatures,
             OverloadedStrings, PolyKinds, UndecidableInstances #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.GHC.TypeLits
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @GHC.TypeLits@ module.
This module is only available with @base-4.6.0.0@ or later.

/Since: 0.5/
-}
module Text.Show.Text.GHC.TypeLits (
#if MIN_VERSION_base(4,7,0)
      showbSomeNatPrec
    , showbSomeSymbol
#else
      showbIsEven
    , showbIsZero
#endif
    ) where

import Data.Text.Lazy.Builder (Builder)
import Prelude hiding (Show)
import Text.Show.Text.Classes (Show(showb, showbPrec))

#if MIN_VERSION_base(4,7,0)
import GHC.TypeLits (SomeNat(..), SomeSymbol(..), natVal, symbolVal)
import Text.Show.Text.Data.Char (showbString)
import Text.Show.Text.Data.Integral (showbIntegerPrec)
#else
import GHC.TypeLits (IsEven(..), IsZero(..), Kind, Sing, SingE(fromSing))
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Utils ((<>), s)
#endif

#if MIN_VERSION_base(4,7,0)
-- | Convert a 'SomeNat' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.7.0.0@ or later.
-- 
-- /Since: 0.5/
showbSomeNatPrec :: Int -> SomeNat -> Builder
showbSomeNatPrec p (SomeNat x) = showbIntegerPrec p $ natVal x
{-# INLINE showbSomeNatPrec #-}

-- | Convert a 'SomeSymbol' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.7.0.0@ or later.
-- 
-- /Since: 0.5/
showbSomeSymbol :: SomeSymbol -> Builder
showbSomeSymbol (SomeSymbol x) = showbString $ symbolVal x
{-# INLINE showbSomeSymbol #-}
#else
-- | Convert an 'IsEven' value to a 'Builder'.
-- This function is only available with @base-4.6@.
-- 
-- /Since: 0.5/
showbIsEven :: IsEven n -> Builder
showbIsEven IsEvenZero = s '0'
showbIsEven (IsEven x) = "(2 * " <> showb x <> s ')'
showbIsEven (IsOdd  x) = "(2 * " <> showb x <> " + 1)"
{-# INLINE showbIsEven #-}

-- | Convert an 'IsZero' value to a 'Builder'.
-- This function is only available with @base-4.6@.
-- 
-- /Since: 0.5/
showbIsZero :: IsZero n -> Builder
showbIsZero IsZero     = s '0'
showbIsZero (IsSucc n) = s '(' <> showb n <> " + 1)"
{-# INLINE showbIsZero #-}
#endif

#if MIN_VERSION_base(4,7,0)
instance Show SomeNat where
    showbPrec = showbSomeNatPrec
    {-# INLINE showbPrec #-}

instance Show SomeSymbol where
    showb = showbSomeSymbol
    {-# INLINE showb #-}
#else
instance Show (IsEven n) where
    showb = showbIsEven
    {-# INLINE showb #-}

instance Show (IsZero n) where
    showb = showbIsZero
    {-# INLINE showb #-}

instance (SingE (Kind :: k) rep, Show rep) => Show (Sing (a :: k)) where
    showbPrec p = showbPrec p . fromSing
    {-# INLINE showbPrec #-}
#endif