{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Data
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @Data.Data@ module.

/Since: 2/
-}
module TextShow.Data.Data () where

import Data.Data (Constr, ConstrRep, DataRep, DataType, Fixity, showConstr)
import Data.Text.Lazy.Builder (fromString)

import TextShow.Classes (TextShow(..))
import TextShow.Data.List ()
import TextShow.Data.Ratio ()
import TextShow.TH.Internal (deriveTextShow)

-- | /Since: 2/
instance TextShow Constr where
    showb = fromString . showConstr
    {-# INLINE showb #-}

-- | /Since: 2/
$(deriveTextShow ''DataRep)
-- | /Since: 2/
$(deriveTextShow ''DataType)
-- | /Since: 2/
$(deriveTextShow ''ConstrRep)
-- | /Since: 2/
$(deriveTextShow ''Fixity)
