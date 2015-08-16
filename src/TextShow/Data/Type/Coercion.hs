{-# LANGUAGE CPP             #-}

#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.Data.Type.Coercion
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for representational equality.
This module only exports functions if using @base-4.7.0.0@ or later.

/Since: 2/
-}
module TextShow.Data.Type.Coercion (
#if !(MIN_VERSION_base(4,7,0))
    ) where
#else
      showbCoercion
    ) where

import Data.Text.Lazy.Builder (Builder)
import Data.Type.Coercion (Coercion(..))

import TextShow.Classes (TextShow(showb, showbPrec), TextShow1(..))
import TextShow.TH.Internal (deriveTextShow2, makeShowbPrec, makeShowbPrecWith)

-- | Convert a representational equality value to a 'Builder'.
-- This function is only available with @base-4.7.0.0@ or later.
--
-- /Since: 2/
showbCoercion :: Coercion a b -> Builder
showbCoercion = showb
{-# INLINE showbCoercion #-}

instance TextShow (Coercion a b) where
    showbPrec = $(makeShowbPrec ''Coercion)
    {-# INLINE showbPrec #-}

instance TextShow1 (Coercion a) where
    showbPrecWith = $(makeShowbPrecWith ''Coercion)
    {-# INLINE showbPrecWith #-}

$(deriveTextShow2 ''Coercion)
#endif
