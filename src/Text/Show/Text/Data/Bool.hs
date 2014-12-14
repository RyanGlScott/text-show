{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Bool
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Bool' values.
-}
module Text.Show.Text.Data.Bool (showbBool) where

import Data.Text.Lazy.Builder (Builder)

import Text.Show.Text.Classes (showb)
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowb)

-- | Convert a 'Bool' to a 'Builder'.
showbBool :: Bool -> Builder
showbBool = showb
{-# INLINE showbBool #-}

$(deriveShowPragmas defaultInlineShowb ''Bool)