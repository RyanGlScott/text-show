{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.GHC.Generics
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for generics-related data types.
This module is only available with @base-4.4.0.0@ or later.

/Since: 0.3/
-}
module Text.Show.Text.GHC.Generics (
      showbU1
    , showbPar1Prec
    , showbRec1Prec
    , showbK1Prec
    , showbM1Prec
    , showbSumTypePrec
    , showbProductTypePrec
    , showbCompFunctorsPrec
    , showbFixityPrec
    , showbAssociativity
    , showbArityPrec
    ) where

import Data.Text.Lazy.Builder (Builder)

import GHC.Generics (U1(..), Par1, Rec1(..), K1(..),
                     M1(..), (:+:)(..), (:*:)(..), (:.:)(..),
                     Fixity, Associativity, Arity)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), Show1(showbPrec1))
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowb,
                                   defaultInlineShowbPrec, mkShowbPrec)

-- | Convert a 'U1' value to a 'Builder'.
-- This function is only available with @base-4.4.0.0@ or later.
-- 
-- /Since: 0.3/
showbU1 :: U1 p -> Builder
showbU1 = showb
{-# INLINE showbU1 #-}

-- | Convert a 'Par1' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
-- 
-- /Since: 0.3/
showbPar1Prec :: Show p => Int -> Par1 p -> Builder
showbPar1Prec = showbPrec
{-# INLINE showbPar1Prec #-}

-- | Convert a 'Rec1' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
-- 
-- /Since: 0.3/
showbRec1Prec :: Show (f p) => Int -> Rec1 f p -> Builder
showbRec1Prec = showbPrec
{-# INLINE showbRec1Prec #-}

-- | Convert a 'K1' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
-- 
-- /Since: 0.3/
showbK1Prec :: Show c => Int -> K1 i c p -> Builder
showbK1Prec = showbPrec
{-# INLINE showbK1Prec #-}

-- | Convert an 'M1' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
-- 
-- /Since: 0.3/
showbM1Prec :: Show (f p) => Int -> M1 i c f p -> Builder
showbM1Prec = showbPrec
{-# INLINE showbM1Prec #-}

-- | Convert a '(:+:)' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
-- 
-- /Since: 0.3/
showbSumTypePrec :: (Show (f p), Show (g p)) => Int -> (f :+: g) p -> Builder
showbSumTypePrec = showbPrec
{-# INLINE showbSumTypePrec #-}

-- | Convert a '(:*:)' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
-- 
-- /Since: 0.3/
showbProductTypePrec :: (Show (f p), Show (g p)) => Int -> (f :*: g) p -> Builder
showbProductTypePrec = showbPrec
{-# INLINE showbProductTypePrec #-}

-- | Convert a '(:.:)' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
-- 
-- /Since: 0.3/
showbCompFunctorsPrec :: Show (f (g p)) => Int -> (f :.: g) p -> Builder
showbCompFunctorsPrec = showbPrec
{-# INLINE showbCompFunctorsPrec #-}

-- | Convert a 'Fixity' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
-- 
-- /Since: 0.3/
showbFixityPrec :: Int -> Fixity -> Builder
showbFixityPrec = showbPrec
{-# INLINE showbFixityPrec #-}

-- | Convert an 'Associativity' value to a 'Builder'.
-- This function is only available with @base-4.4.0.0@ or later.
-- 
-- /Since: 0.3/
showbAssociativity :: Associativity -> Builder
showbAssociativity = showb
{-# INLINE showbAssociativity #-}

-- | Convert an 'Arity' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
-- 
-- /Since: 0.3/
showbArityPrec :: Int -> Arity -> Builder
showbArityPrec = showbPrec
{-# INLINE showbArityPrec #-}

-- TODO: Derive with TH once it can detect phantom types properly
instance Show (U1 p) where
    showbPrec = $(mkShowbPrec ''U1)
    {-# INLINE showb #-}

instance Show1 U1 where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

$(deriveShowPragmas defaultInlineShowbPrec ''Par1)

instance Show1 Par1 where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

-- TODO: Derive with TH once it can detect higher-kinded types properly
instance Show (f p) => Show (Rec1 f p) where
    showbPrec = $(mkShowbPrec ''Rec1)
    {-# INLINE showbPrec #-}

-- TODO: Derive with TH once it can detect phantom types properly
instance Show c => Show (K1 i c p) where
    showbPrec = $(mkShowbPrec ''K1)
    {-# INLINE showbPrec #-}

-- TODO: Derive with TH once it can detect phantom types properly
instance Show c => Show1 (K1 i c) where
    showbPrec1 = showbPrec
    {-# INLINE showbPrec1 #-}

-- TODO: Derive with TH once it can detect phantom types
-- and higher-kinded types properly
instance Show (f p) => Show (M1 i c f p) where
    showbPrec = $(mkShowbPrec ''M1)
    {-# INLINE showbPrec #-}

-- TODO: Derive with TH once it can detect higher-kinded types properly
instance (Show (f p), Show (g p)) => Show ((f :+: g) p) where
    showbPrec = $(mkShowbPrec ''(:+:))
    {-# INLINE showbPrec #-}

-- TODO: Derive with TH once it can detect higher-kinded types properly
instance (Show (f p), Show (g p)) => Show ((f :*: g) p) where
    showbPrec = $(mkShowbPrec ''(:*:))
    {-# INLINE showbPrec #-}

-- TODO: Derive with TH once it can detect higher-kinded types properly
instance Show (f (g p)) => Show ((f :.: g) p) where
    showbPrec = $(mkShowbPrec ''(:.:))
    {-# INLINE showbPrec #-}

$(deriveShowPragmas defaultInlineShowbPrec ''Fixity)
$(deriveShowPragmas defaultInlineShowb     ''Associativity)
$(deriveShowPragmas defaultInlineShowbPrec ''Arity)
