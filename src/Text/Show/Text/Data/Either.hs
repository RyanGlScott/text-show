{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Either
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' function for 'Either' values.

/Since: 0.3/
-}
module Text.Show.Text.Data.Either (showbEitherPrecWith2) where

import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (showbPrecWith2)
import Text.Show.Text.TH.Internal (deriveShow, deriveShow1, deriveShow2)

#include "inline.h"

-- | Convert a 'Either' value to a 'Builder' with the given show functions
-- and precedence.
--
-- /Since: 1/
showbEitherPrecWith2 :: (Int -> a -> Builder) -> (Int -> b -> Builder)
                     -> Int -> Either a b -> Builder
showbEitherPrecWith2 = showbPrecWith2
{-# INLINE showbEitherPrecWith2 #-}

$(deriveShow  ''Either)
$(deriveShow1 ''Either)
$(deriveShow2 ''Either)
