{-# LANGUAGE CPP             #-}

#if !defined(__GHCJS__) && defined(mingw32_HOST_OS)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      Text.Show.Text.GHC.Conc.Windows
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'ConsoleEvent'.
This module only exports functions if using Windows, and not using GHCJS.

/Since: 0.5/
-}
module Text.Show.Text.GHC.Conc.Windows (
#if defined(__GHCJS__) || !defined(mingw32_HOST_OS)
    ) where
#else
      showbConsoleEvent
    ) where

import Data.Text.Lazy.Builder (Builder)

import GHC.Conc.Windows (ConsoleEvent)

import Text.Show.Text.Classes (showb)
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowb)

-- | Convert a 'ConsoleEvent' to a 'Builder'.
-- 
-- /Since: 0.5/
showbConsoleEvent :: ConsoleEvent -> Builder
showbConsoleEvent = showb
{-# INLINE showbConsoleEvent #-}

$(deriveShowPragmas defaultInlineShowb ''ConsoleEvent)
#endif
