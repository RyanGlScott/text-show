{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
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

import Data.Data (Constr, ConstrRep, DataRep, DataType, Fixity, showConstr)
import Data.Text.Lazy.Builder (Builder, fromString)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb, showbPrec))
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Data.List ()
import Text.Show.Text.TH.Internal (deriveShow)

-- | Convert a 'DataType' to a 'Builder' with the given precedence.
showbDataTypePrec :: Int -> DataType -> Builder
showbDataTypePrec = showbPrec
{-# INLINE showbDataTypePrec #-}

-- | Convert a 'DataRep' to a 'Builder' with the given precedence.
showbDataRepPrec :: Int -> DataRep -> Builder
showbDataRepPrec = showbPrec
{-# INLINE showbDataRepPrec #-}

-- | Convert a 'Constr' to a 'Builder'.
showbConstr :: Constr -> Builder
showbConstr = fromString . showConstr
{-# INLINE showbConstr #-}

-- | Convert a 'Fixity' value to a 'Builder'.
showbFixity :: Fixity -> Builder
showbFixity = showb
{-# INLINE showbFixity #-}

-- | Convert a 'ConstrRep' to a 'Builder' with the given precedence.
showbConstrRepPrec :: Int -> ConstrRep -> Builder
showbConstrRepPrec = showbPrec
{-# INLINE showbConstrRepPrec #-}

$(deriveShow ''DataType)
$(deriveShow ''DataRep)
$(deriveShow ''ConstrRep)
$(deriveShow ''Fixity)

instance Show Constr where
    showb = showbConstr
    {-# INLINE showb #-}