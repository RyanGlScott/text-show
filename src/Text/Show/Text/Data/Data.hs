{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Data
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for data types in the @Data@ module.
----------------------------------------------------------------------------
module Text.Show.Text.Data.Data (
      showbConstr
    , showbConstrRepPrec
    , showbDataRepPrec
    , showbDataTypePrec
    , showbFixity
    ) where

import Data.Data (Constr, ConstrRep(..), DataRep(..), DataType, Fixity(..),
                  dataTypeName, dataTypeRep, showConstr)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (Builder, fromString)

import GHC.Show (appPrec, appPrec1)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb, showbPrec), showbParen)
import Text.Show.Text.Data.Char (showbChar)
import Text.Show.Text.Data.Integral (showbIntPrec, showbIntegerPrec, showbRatioPrec)
import Text.Show.Text.Data.List ()
import Text.Show.Text.Utils (s)

-- | Convert a 'DataType' to a 'Builder' with the given precedence.
showbDataTypePrec :: Int -> DataType -> Builder
showbDataTypePrec p dt = showbParen (p > appPrec) $
       "DataType {tycon = "
    <> showb (dataTypeName dt)
    <> ", datarep = "
    <> showb (dataTypeRep  dt)
    <> s '}'
{-# INLINE showbDataTypePrec #-}

-- | Convert a 'DataRep' to a 'Builder' with the given precedence.
showbDataRepPrec :: Int -> DataRep -> Builder
showbDataRepPrec p (AlgRep cs) = showbParen (p > appPrec) $ "AlgRep " <> showb cs
showbDataRepPrec _ IntRep      = "IntRep"
showbDataRepPrec _ FloatRep    = "FloatRep"
showbDataRepPrec _ CharRep     = "CharRep"
showbDataRepPrec _ NoRep       = "NoRep"
{-# INLINE showbDataRepPrec #-}

-- | Convert a 'Constr' to a 'Builder'.
showbConstr :: Constr -> Builder
showbConstr = fromString . showConstr
{-# INLINE showbConstr #-}

-- | Convert a 'Fixity' value to a 'Builder'.
showbFixity :: Fixity -> Builder
showbFixity Prefix = "Prefix"
showbFixity Infix  = "Infix"
{-# INLINE showbFixity #-}

-- | Convert a 'ConstrRep' to a 'Builder' with the given precedence.
showbConstrRepPrec :: Int -> ConstrRep -> Builder
showbConstrRepPrec p (AlgConstr ci)
    = showbParen (p > appPrec) $ "AlgConstr "   <> showbIntPrec appPrec1 ci
showbConstrRepPrec p (IntConstr i)
    = showbParen (p > appPrec) $ "IntConstr "   <> showbIntegerPrec appPrec1 i
showbConstrRepPrec p (FloatConstr r)
    = showbParen (p > appPrec) $ "FloatConstr " <> showbRatioPrec appPrec1 r
showbConstrRepPrec p (CharConstr c)
    = showbParen (p > appPrec) $ "CharConstr "  <> showbChar c
{-# INLINE showbConstrRepPrec #-}

instance Show DataType where
    showbPrec = showbDataTypePrec
    {-# INLINE showbPrec #-}

instance Show DataRep where
    showbPrec = showbDataRepPrec
    {-# INLINE showbPrec #-}

instance Show Constr where
    showb = showbConstr
    {-# INLINE showb #-}

instance Show ConstrRep where
    showbPrec = showbConstrRepPrec
    {-# INLINE showbPrec #-}

instance Show Fixity where
    showb = showbFixity
    {-# INLINE showb #-}