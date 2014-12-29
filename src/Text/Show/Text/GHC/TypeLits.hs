{-# LANGUAGE CPP #-}
#if !(MIN_VERSION_base(4,7,0))
{-# LANGUAGE OverloadedStrings #-}
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

#if MIN_VERSION_base(4,7,0)
import GHC.TypeLits (SomeNat(..), SomeSymbol(..), natVal, symbolVal)

import Text.Show.Text.Classes (showbPrec)
import Text.Show.Text.Data.Char (showbString)
#else
import GHC.TypeLits (IsEven(..), IsZero(..))

import Text.Show.Text.Utils ((<>), s)
#endif

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb))
import Text.Show.Text.Data.Integral (showbIntegerPrec)

#if MIN_VERSION_base(4,7,0)
-- | Convert a 'SomeNat' value to a 'Builder' with the given precedence.
showbSomeNatPrec :: Int -> SomeNat -> Builder
showbSomeNatPrec p (SomeNat x) = showbIntegerPrec p $ natVal x
{-# INLINE showbSomeNatPrec #-}

-- | Convert a 'SomeSymbol' value to a 'Builder' with the given precedence.
showbSomeSymbol :: SomeSymbol -> Builder
showbSomeSymbol (SomeSymbol x) = showbString $ symbolVal x
{-# INLINE showbSomeSymbol #-}
#else
-- | Convert an 'IsEven' value to a 'Builder'.
showbIsEven :: IsEven n -> Builder
showbIsEven IsEvenZero = s '0'
showbIsEven (IsEven x) = "(2 * " <> showbIntegerPrec 0 x <> s ')'
showbIsEven (IsOdd  x) = "(2 * " <> showbIntegerPrec 0 x <> " + 1)"
{-# INLINE showbIsEven #-}

-- | Convert an 'IsZero' value to a 'Builder'.
showbIsZero :: IsZero n -> Builder
showbIsZero IsZero     = s '0'
showbIsZero (IsSucc n) = s '(' <> showbIntegerPrec n <> " + 1)"
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
#endif