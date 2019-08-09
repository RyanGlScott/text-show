{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Void
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Void'.

/Since: 2/
-}
module TextShow.Data.Void () where

import Data.Void.Compat (Void, absurd)
import Prelude ()
import TextShow.Classes (TextShow(..))

-- | /Since: 2/
instance TextShow Void where
    showb = absurd
