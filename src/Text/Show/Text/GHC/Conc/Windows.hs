{-# LANGUAGE CPP #-}
#if !defined(__GHCJS__)
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.GHC.Conc.Windows
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'ConsoleEvent'.
This module is only available on Windows.

Note that this module does not export anything on GHCJS.

/Since: 0.5/
-}
module Text.Show.Text.GHC.Conc.Windows (
#if defined(__GHCJS__)
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