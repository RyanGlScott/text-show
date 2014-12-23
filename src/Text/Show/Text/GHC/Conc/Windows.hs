{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.GHC.Conc.Windows
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'ConsoleEvent'.
-}
module Text.Show.Text.GHC.Conc.Windows (
#if defined(__GHCJS__)
    ) where
#else
      showbConsoleEvent
    ) where
#endif

import Data.Text.Lazy.Builder (Builder)

import GHC.Conc.Windows (ConsoleEvent)

import Text.Show.Text.Classes (showb)
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowb)

-- | Convert a 'ConsoleEvent' to a 'Builder'.
showbConsoleEvent :: ConsoleEvent -> Builder
showbConsoleEvent = showb
{-# INLINE showbConsoleEvent #-}

$(deriveShowPragmas defaultInlineShowb ''ConsoleEvent)