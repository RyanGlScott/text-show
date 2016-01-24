{-# LANGUAGE CPP             #-}

#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.Data.Type.Equality
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for propositional equality.
This module only exports functions if using @base-4.7.0.0@ or later.

/Since: 2/
-}
module TextShow.Data.Type.Equality (
#if !(MIN_VERSION_base(4,7,0))
    ) where
#else
      showbPropEquality
    ) where

import Data.Text.Lazy.Builder (Builder)
import Data.Type.Equality ((:~:)(..))

import TextShow.Classes (TextShow(..), TextShow1(..))
import TextShow.TH.Internal (deriveTextShow, deriveTextShow2, makeShowbPrecWith)

-- | Convert a propositional equality value to a 'Builder'.
-- This function is only available with @base-4.7.0.0@ or later.
--
-- /Since: 2/
showbPropEquality :: (a :~: b) -> Builder
showbPropEquality = showb
{-# INLINE showbPropEquality #-}

$(deriveTextShow ''(:~:))

instance TextShow1 ((:~:) a) where
    showbPrecWith = $(makeShowbPrecWith ''(:~:))
    {-# INLINE showbPrecWith #-}

$(deriveTextShow2 ''(:~:))
#endif
