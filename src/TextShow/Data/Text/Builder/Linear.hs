{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Data.Text
Copyright:   (C) 2026 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines a 'TextShow' instance for @text-builder-linear@'s 'Builder' type.

/Since: TODO RGS/
-}
module TextShow.Data.Text.Builder.Linear () where

import Data.Text.Builder.Linear (Builder(..))
import Data.Text.Builder.Linear.Core (runBuffer)

import TextShow.Classes (TextShow(..))
import TextShow.Data.Text ()

-- | /Since: TODO RGS/
instance TextShow Builder where
  showb (Builder f) = showb (runBuffer f)
