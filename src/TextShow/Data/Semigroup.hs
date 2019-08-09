{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.Semigroup
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the "Data.Semigroup" module.

/Since: 3/
-}
module TextShow.Data.Semigroup () where

import Data.Semigroup.Compat (Min, Max, First, Last, WrappedMonoid, Option, Arg)

import TextShow.Data.Maybe ()
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1, deriveTextShow2)

-- | /Since: 3/
$(deriveTextShow  ''Min)
-- | /Since: 3/
$(deriveTextShow1 ''Min)

-- | /Since: 3/
$(deriveTextShow  ''Max)
-- | /Since: 3/
$(deriveTextShow1 ''Max)

-- | /Since: 3/
$(deriveTextShow  ''First)
-- | /Since: 3/
$(deriveTextShow1 ''First)

-- | /Since: 3/
$(deriveTextShow  ''Last)
-- | /Since: 3/
$(deriveTextShow1 ''Last)

-- | /Since: 3/
$(deriveTextShow  ''WrappedMonoid)
-- | /Since: 3/
$(deriveTextShow1 ''WrappedMonoid)

-- | /Since: 3/
$(deriveTextShow  ''Option)
-- | /Since: 3/
$(deriveTextShow1 ''Option)

-- | /Since: 3/
$(deriveTextShow  ''Arg)
-- | /Since: 3/
$(deriveTextShow1 ''Arg)
-- | /Since: 3/
$(deriveTextShow2 ''Arg)
