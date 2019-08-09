{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.List.NonEmpty
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'NonEmpty' lists.

/Since: 3/
-}
module TextShow.Data.List.NonEmpty () where

import Data.List.NonEmpty.Compat (NonEmpty)

import TextShow.Data.List ()
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1)

-- | /Since: 3/
$(deriveTextShow  ''NonEmpty)
-- | /Since: 3/
$(deriveTextShow1 ''NonEmpty)
