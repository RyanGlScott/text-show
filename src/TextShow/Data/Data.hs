{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Data
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @Data.Data@ module.

/Since: 2/
-}
module TextShow.Data.Data (
      showbConstr
    , showbConstrRepPrec
    , showbDataRepPrec
    , showbDataTypePrec
    , showbFixity
    ) where

import Data.Data (Constr, ConstrRep, DataRep, DataType, Fixity, showConstr)
import Data.Text.Lazy.Builder (Builder, fromString)

import TextShow.Classes (TextShow(showb, showbPrec))
import TextShow.Data.List ()
import TextShow.Data.Ratio ()
import TextShow.TH.Internal (deriveTextShow)

#include "inline.h"

-- | Convert a 'DataType' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbDataTypePrec :: Int -> DataType -> Builder
showbDataTypePrec = showbPrec
{-# INLINE showbDataTypePrec #-}

-- | Convert a 'DataRep' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbDataRepPrec :: Int -> DataRep -> Builder
showbDataRepPrec = showbPrec
{-# INLINE showbDataRepPrec #-}

-- | Convert a 'Constr' to a 'Builder'.
--
-- /Since: 2/
showbConstr :: Constr -> Builder
showbConstr = fromString . showConstr
{-# INLINE showbConstr #-}

-- | Convert a 'Fixity' value to a 'Builder'.
--
-- /Since: 2/
showbFixity :: Fixity -> Builder
showbFixity = showb
{-# INLINE showbFixity #-}

-- | Convert a 'ConstrRep' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbConstrRepPrec :: Int -> ConstrRep -> Builder
showbConstrRepPrec = showbPrec
{-# INLINE showbConstrRepPrec #-}

$(deriveTextShow ''DataType)
$(deriveTextShow ''DataRep)
$(deriveTextShow ''ConstrRep)
$(deriveTextShow ''Fixity)

instance TextShow Constr where
    showb = showbConstr
    INLINE_INST_FUN(showb)
