{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Floating
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for floating-point types.
----------------------------------------------------------------------------
module Text.Show.Text.Data.Floating (
      showbRealFloatPrec
    , showbFloatPrec
    , showbDoublePrec
    , showbComplexPrec
    , showbEFloat
    , showbFFloat
    , showbGFloat
    ) where

import Data.Complex (Complex(..))
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder.RealFloat (FPFormat(..), formatRealFloat, realFloat)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showbPrec), showbParen)
import Text.Show.Text.Utils (s)

-- | Convert a 'RealFloat' value to a 'Builder' with the given precedence.
showbRealFloatPrec :: RealFloat a => Int -> a -> Builder
showbRealFloatPrec p x
    | x < 0 || isNegativeZero x = showbParen (p > 6) $ s '-' <> realFloat (-x)
    | otherwise                 = realFloat x
{-# INLINE showbRealFloatPrec #-}

-- | Convert a 'Float' to a 'Builder' with the given precedence.
showbFloatPrec :: Int -> Float -> Builder
showbFloatPrec = showbRealFloatPrec
{-# INLINE showbFloatPrec #-}

-- | Convert a 'Double' to a 'Builder' with the given precedence.
showbDoublePrec :: Int -> Double -> Builder
showbDoublePrec = showbRealFloatPrec
{-# INLINE showbDoublePrec #-}

-- | Convert a 'Complex' value to a 'Builder' with the given precedence.
showbComplexPrec :: (Show a, RealFloat a) => Int -> Complex a -> Builder
showbComplexPrec p (a :+ b) = showbParen (p > complexPrec) $
        showbPrec (complexPrec+1) a
     <> " :+ "
     <> showbPrec (complexPrec+1) b
  where complexPrec = 6
{-# INLINE showbComplexPrec #-}

-- | Show a signed 'RealFloat' value
-- using scientific (exponential) notation (e.g. @2.45e2@, @1.5e-3@).
--
-- In the call @'showbEFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then at most @d@ digits after the decimal point are shown.
showbEFloat :: RealFloat a => Maybe Int -> a -> Builder
showbEFloat = formatRealFloat Exponent
{-# INLINE showbEFloat #-}

-- | Show a signed 'RealFloat' value
-- using standard decimal notation (e.g. @245000@, @0.0015@).
--
-- In the call @'showbFFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then at most @d@ digits after the decimal point are shown.
showbFFloat :: RealFloat a => Maybe Int -> a -> Builder
showbFFloat = formatRealFloat Fixed
{-# INLINE showbFFloat #-}


-- | Show a signed 'RealFloat' value
-- using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
--
-- In the call @'showbGFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then at most @d@ digits after the decimal point are shown.
showbGFloat :: RealFloat a => Maybe Int -> a -> Builder
showbGFloat = formatRealFloat Generic
{-# INLINE showbGFloat #-}

instance Show Float where
    showbPrec = showbFloatPrec
    {-# INLINE showbPrec #-}

instance Show Double where
    showbPrec = showbDoublePrec
    {-# INLINE showbPrec #-}

instance (Show a, RealFloat a) => Show (Complex a) where
    {-# SPECIALIZE instance Show (Complex Float) #-}
    {-# SPECIALIZE instance Show (Complex Double) #-}
    showbPrec = showbComplexPrec
    {-# INLINE showbPrec #-}