{-# LANGUAGE CPP             #-}

#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.Data.Type.Coercion
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for representational equality.
Only provided if using @base-4.7.0.0@ or later.

/Since: 2/
-}
module TextShow.Data.Type.Coercion () where

#if MIN_VERSION_base(4,7,0)
import Data.Type.Coercion (Coercion)

import TextShow.Classes (TextShow1(..))
import TextShow.TH.Internal (deriveTextShow, deriveTextShow2, makeLiftShowbPrec)

-- | /Since: 2/
$(deriveTextShow ''Coercion)

-- | /Since: 2/
instance TextShow1 (Coercion a) where
    liftShowbPrec = $(makeLiftShowbPrec ''Coercion)

-- | /Since: 2/
$(deriveTextShow2 ''Coercion)
#endif
