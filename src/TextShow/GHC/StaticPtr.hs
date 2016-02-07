{-# LANGUAGE CPP             #-}

#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.GHC.StaticPtr
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'StaticPtrInfo' values.
This module only exports functions if using @base-4.8.0.0@ or later.

/Since: 2/
-}
module TextShow.GHC.StaticPtr (
#if !(MIN_VERSION_base(4,8,0))
    ) where
#else
      showbStaticPtrInfoPrec
    ) where

import Data.Text.Lazy.Builder (Builder)

import GHC.StaticPtr (StaticPtrInfo)

import TextShow.Classes (showbPrec)
import TextShow.Data.Char     ()
import TextShow.Data.Integral ()
import TextShow.Data.List     ()
import TextShow.Data.Tuple    ()
import TextShow.TH.Internal (deriveTextShow)

-- | Conver a 'StaticPtrInfo' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
showbStaticPtrInfoPrec :: Int -> StaticPtrInfo -> Builder
showbStaticPtrInfoPrec = showbPrec
{-# INLINE showbStaticPtrInfoPrec #-}

$(deriveTextShow ''StaticPtrInfo)
#endif
