{-# LANGUAGE CPP             #-}

#if !defined(__GHCJS__) && defined(mingw32_HOST_OS)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.GHC.Conc.Windows
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'ConsoleEvent'.
This module only exports functions if using Windows, and not using GHCJS.

/Since: 2/
-}
module TextShow.GHC.Conc.Windows (
#if defined(__GHCJS__) || !defined(mingw32_HOST_OS)
    ) where
#else
      showbConsoleEvent
    ) where

import Data.Text.Lazy.Builder (Builder)

import GHC.Conc.Windows (ConsoleEvent)

import TextShow.Classes (showb)
import TextShow.TH.Internal (deriveTextShow)

-- | Convert a 'ConsoleEvent' to a 'Builder'.
--
-- /Since: 2/
showbConsoleEvent :: ConsoleEvent -> Builder
showbConsoleEvent = showb
{-# INLINE showbConsoleEvent #-}

$(deriveTextShow ''ConsoleEvent)
#endif
