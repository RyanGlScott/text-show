{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Data
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Data.Data@ module.

/Since: 0.3/
-}
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

import Text.Show.Text.Classes (Show(showb, showbPrec))
import Text.Show.Text.Data.List ()
import Text.Show.Text.Data.Ratio ()
import Text.Show.Text.TH.Internal (deriveShow)

#include "inline.h"

-- | Convert a 'DataType' to a 'Builder' with the given precedence.
--
-- /Since: 0.3/
showbDataTypePrec :: Int -> DataType -> Builder
showbDataTypePrec = showbPrec
{-# INLINE showbDataTypePrec #-}

-- | Convert a 'DataRep' to a 'Builder' with the given precedence.
--
-- /Since: 0.3/
showbDataRepPrec :: Int -> DataRep -> Builder
showbDataRepPrec = showbPrec
{-# INLINE showbDataRepPrec #-}

-- | Convert a 'Constr' to a 'Builder'.
--
-- /Since: 0.3/
showbConstr :: Constr -> Builder
showbConstr = fromString . showConstr
{-# INLINE showbConstr #-}

-- | Convert a 'Fixity' value to a 'Builder'.
--
-- /Since: 0.3/
showbFixity :: Fixity -> Builder
showbFixity = showb
{-# INLINE showbFixity #-}

-- | Convert a 'ConstrRep' to a 'Builder' with the given precedence.
--
-- /Since: 0.3/
showbConstrRepPrec :: Int -> ConstrRep -> Builder
showbConstrRepPrec = showbPrec
{-# INLINE showbConstrRepPrec #-}

$(deriveShow ''DataType)
$(deriveShow ''DataRep)
$(deriveShow ''ConstrRep)
$(deriveShow ''Fixity)

instance Show Constr where
    showb = showbConstr
    INLINE_INST_FUN(showb)
