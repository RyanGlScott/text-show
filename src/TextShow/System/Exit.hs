{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.System.Exit
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'ExitCode'.

/Since: 2/
-}
module TextShow.System.Exit () where

import System.Exit (ExitCode)

import TextShow.Data.Integral ()
import TextShow.TH.Internal (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''ExitCode)
