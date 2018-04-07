{-# LANGUAGE CPP             #-}

#if !defined(__GHCJS__) && !defined(mingw32_HOST_OS)
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.GHC.Event
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @Event@ module.
Only provided if using a platform other than Windows or GHCJS.

/Since: 2/
-}
module TextShow.GHC.Event () where

#if !defined(__GHCJS__) && !defined(mingw32_HOST_OS)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Data.Text.Lazy.Builder (Builder, singleton)

import GHC.Event (Event, evtRead, evtWrite)

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

-- | /Since: 2/
instance TextShow Event where
    showb e = singleton '[' <> mconcat (intersperse "," $ catMaybes
        [ evtRead                 `so` "evtRead"
        , evtWrite                `so` "evtWrite"
        , $(varE evtCloseValName) `so` "evtClose"
        ]) <> singleton ']'
      where
        so :: Event -> Builder -> Maybe Builder
        ev `so` disp | $(varE eventIsValName) e ev = Just disp
                     | otherwise                   = Nothing

-- | /Since: 2/
$(deriveTextShow fdKeyTypeName)

instance TextShow $(conT uniqueTypeName) where
    showb = showb . $(varE asInt64ValName)
    {-# INLINE showb #-}

# if MIN_VERSION_base(4,8,1)
-- | Only available with @base-4.8.1.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''Lifetime)
# endif
#endif
