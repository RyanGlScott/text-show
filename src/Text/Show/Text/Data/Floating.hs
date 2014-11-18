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
    ) where

import Data.Complex (Complex(..))
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder.RealFloat (realFloat)

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

-- TODO: showbEFloat, showbFFloat, showbGFloat

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