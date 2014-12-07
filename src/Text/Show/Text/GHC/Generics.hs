{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, OverloadedStrings,
             TemplateHaskell, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.GHC.Generics
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for generics-related data types.
----------------------------------------------------------------------------
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
import GHC.Show (appPrec, appPrec1)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb, showbPrec), showbParen)
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.TH.Internal (deriveShow)
import Text.Show.Text.Utils ((<>), s)

-- | Convert a 'U1' value to a 'Builder'.
showbU1 :: U1 p -> Builder
showbU1 U1 = "U1"
{-# INLINE showbU1 #-}

-- | Convert a 'Par1' value to a 'Builder' with the given precedence.
showbPar1Prec :: Show p => Int -> Par1 p -> Builder
showbPar1Prec = showbPrec
{-# INLINE showbPar1Prec #-}

-- | Convert a 'Rec1' value to a 'Builder' with the given precedence.
showbRec1Prec :: Show (f p) => Int -> Rec1 f p -> Builder
showbRec1Prec p (Rec1 ur) = showbParen (p > appPrec) $
    "Rec1 {unRec1 = " <> showb ur <> s '}'
{-# INLINE showbRec1Prec #-}

-- | Convert a 'K1' value to a 'Builder' with the given precedence.
showbK1Prec :: Show c => Int -> K1 i c p -> Builder
showbK1Prec p (K1 uk) = showbParen (p > appPrec) $
    "K1 {unK1 = " <> showb uk <> s '}'
{-# INLINE showbK1Prec #-}

-- | Convert an 'M1' value to a 'Builder' with the given precedence.
showbM1Prec :: Show (f p) => Int -> M1 i c f p -> Builder
showbM1Prec p (M1 um) = showbParen (p > appPrec) $
    "M1 {unM1 = " <> showb um <> s '}'
{-# INLINE showbM1Prec #-}

-- | Convert a '(:+:)' value to a 'Builder' with the given precedence.
showbSumTypePrec :: (Show (f p), Show (g p)) => Int -> (f :+: g) p -> Builder
showbSumTypePrec p (L1 l) = showbParen (p > appPrec) $ "L1 " <> showbPrec appPrec1 l
showbSumTypePrec p (R1 r) = showbParen (p > appPrec) $ "R1 " <> showbPrec appPrec1 r
{-# INLINE showbSumTypePrec #-}

-- | Convert an '(:*:)' value to a 'Builder' with the given precedence.
showbProductTypePrec :: (Show (f p), Show (g p)) => Int -> (f :*: g) p -> Builder
showbProductTypePrec p (l :*: r) = showbParen (p > prec) $
       showbPrec (prec + 1) l
    <> " :*: "
    <> showbPrec (prec + 1) r
  where
    prec :: Int
    prec = 6
{-# INLINE showbProductTypePrec #-}

-- | Convert an '(:.:)' value to a 'Builder' with the given precedence.
showbCompFunctorsPrec :: Show (f (g p)) => Int -> (f :.: g) p -> Builder
showbCompFunctorsPrec p (Comp1 uc) = showbParen (p > appPrec) $
    "Comp1 {unComp1 = " <> showb uc <> s '}'
{-# INLINE showbCompFunctorsPrec #-}

-- | Convert a 'Fixity' value to a 'Builder' with the given precedence.
showbFixityPrec :: Int -> Fixity -> Builder
showbFixityPrec = showbPrec
{-# INLINE showbFixityPrec #-}

-- | Convert an 'Associativity' value to a 'Builder'.
showbAssociativity :: Associativity -> Builder
showbAssociativity = showb
{-# INLINE showbAssociativity #-}

-- | Convert an 'Arity' value to a 'Builder' with the given precedence.
showbArityPrec :: Int -> Arity -> Builder
showbArityPrec = showbPrec
{-# INLINE showbArityPrec #-}

instance Show (U1 p) where
    showb = showbU1
    {-# INLINE showb #-}

$(deriveShow ''Par1)

-- TODO: 'deriveShow' is not smart enough to derive higher-kinded type contexts
instance Show (f p) => Show (Rec1 f p) where
    showbPrec = showbRec1Prec
    {-# INLINE showbPrec #-}

instance Show c => Show (K1 i c p) where
    showbPrec = showbK1Prec
    {-# INLINE showbPrec #-}

instance Show (f p) => Show (M1 i c f p) where
    showbPrec = showbM1Prec
    {-# INLINE showbPrec #-}

instance (Show (f p), Show (g p)) => Show ((f :+: g) p) where
    showbPrec = showbSumTypePrec
    {-# INLINE showbPrec #-}

instance (Show (f p), Show (g p)) => Show ((f :*: g) p) where
    showbPrec = showbProductTypePrec
    {-# INLINE showbPrec #-}

instance Show (f (g p)) => Show ((f :.: g) p) where
    showbPrec = showbCompFunctorsPrec
    {-# INLINE showbPrec #-}

$(deriveShow ''Fixity)
$(deriveShow ''Associativity)
$(deriveShow ''Arity)