{-# LANGUAGE CPP #-}

#if !defined(__GHCJS__) && !defined(mingw32_HOST_OS) && MIN_VERSION_base(4,4,0)
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
    ) where

import Data.Text.Lazy.Builder (Builder)
import GHC.Event (Event, FdKey)
import TextShow.Classes (TextShow(showb, showbPrec), FromStringShow(..))

-- | Convert an 'Event' to a 'Builder'.
-- This function is only available with @base-4.4.0.0@ or later and is not available
-- on Windows.
--
-- /Since: 2/
showbEvent :: Event -> Builder
showbEvent = showb . FromStringShow
{-# INLINE showbEvent #-}

-- | Convert an 'FdKey' to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later and is not available
-- on Windows.
--
-- /Since: 2/
showbFdKeyPrec :: Int -> FdKey -> Builder
showbFdKeyPrec p = showbPrec p . FromStringShow
{-# INLINE showbFdKeyPrec #-}

instance TextShow Event where
    showb = showbEvent
    {-# INLINE showb #-}

instance TextShow FdKey where
    showbPrec = showbFdKeyPrec
    {-# INLINE showbPrec #-}
#endif
