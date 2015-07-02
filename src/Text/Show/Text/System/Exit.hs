{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.System.Exit
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' function for 'ExitCode'.

/Since: 0.3/
-}
module Text.Show.Text.System.Exit (showbExitCodePrec) where

import Data.Text.Lazy.Builder (Builder)

import System.Exit (ExitCode)

import Text.Show.Text.Classes (showbPrec)
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.TH.Internal (deriveShow)

-- | Convert an 'ExitCode' to a 'Builder' with the given precedence.
--
-- /Since: 0.3/
showbExitCodePrec :: Int -> ExitCode -> Builder
showbExitCodePrec = showbPrec
{-# INLINE showbExitCodePrec #-}

$(deriveShow ''ExitCode)
