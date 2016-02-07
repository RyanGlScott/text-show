{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.System.Exit
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'ExitCode'.

/Since: 2/
-}
module TextShow.System.Exit (showbExitCodePrec) where

import Data.Text.Lazy.Builder (Builder)

import System.Exit (ExitCode)

import TextShow.Classes (showbPrec)
import TextShow.Data.Integral ()
import TextShow.TH.Internal (deriveTextShow)

-- | Convert an 'ExitCode' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbExitCodePrec :: Int -> ExitCode -> Builder
showbExitCodePrec = showbPrec
{-# INLINE showbExitCodePrec #-}

$(deriveTextShow ''ExitCode)
