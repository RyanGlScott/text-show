{-# LANGUAGE CPP             #-}

#if !defined(__GHCJS__) && !defined(mingw32_HOST_OS) && MIN_VERSION_base(4,4,0)
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.GHC.Event
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @Event@ module.
This module only exports functions if using @base-4.4.0.0@ on a platform other
than Windows or GHCJS.

/Since: 2/
-}
module TextShow.GHC.Event (
#if defined(__GHCJS__) || defined(mingw32_HOST_OS) || !(MIN_VERSION_base(4,4,0))
    ) where
#else
      showbEvent
    , showbFdKeyPrec
# if MIN_VERSION_base(4,8,1)
    , showbLifetime
# endif
    ) where

import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (Builder, singleton)

import GHC.Event (Event, FdKey, evtRead, evtWrite)

import Language.Haskell.TH.Lib (conT, varE)

import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(..))
import TextShow.Data.Integral      ()
import TextShow.System.Posix.Types ()
import TextShow.TH.Internal (deriveTextShow)
import TextShow.TH.Names (evtCloseValName, eventIsValName,
                          fdKeyTypeName, uniqueTypeName, asInt64ValName)

# if MIN_VERSION_base(4,8,1)
import GHC.Event (Lifetime)
# endif

#include "inline.h"

-- | Convert an 'Event' to a 'Builder'.
-- This function is only available with @base-4.4.0.0@ or later and is not available
-- on Windows.
--
-- /Since: 2/
showbEvent :: Event -> Builder
showbEvent e = singleton '[' <> mconcat (intersperse "," $ catMaybes
    [ evtRead                 `so` "evtRead"
    , evtWrite                `so` "evtWrite"
    , $(varE evtCloseValName) `so` "evtClose"
    ]) <> singleton ']'
  where
    so :: Event -> Builder -> Maybe Builder
    ev `so` disp | $(varE eventIsValName) e ev = Just disp
                 | otherwise                   = Nothing

-- | Convert an 'FdKey' to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later and is not available
-- on Windows.
--
-- /Since: 2/
showbFdKeyPrec :: Int -> FdKey -> Builder
showbFdKeyPrec = showbPrec
{-# INLINE showbFdKeyPrec #-}

# if MIN_VERSION_base(4,8,1)
-- | Convert a 'Lifetime' to a 'Builder'.
-- This function is only available with @base-4.8.1.0@ or later and is not available
-- on Windows.
--
-- /Since: 2/
showbLifetime :: Lifetime -> Builder
showbLifetime = showb
{-# INLINE showbLifetime #-}
# endif

instance TextShow Event where
    showb = showbEvent
    {-# INLINE showb #-}

$(deriveTextShow fdKeyTypeName)

instance TextShow $(conT uniqueTypeName) where
    showb = showb . $(varE asInt64ValName)
    INLINE_INST_FUN(showb)

# if MIN_VERSION_base(4,8,1)
$(deriveTextShow ''Lifetime)
# endif
#endif
